{-# LANGUAGE OverloadedStrings #-}

module Compare 
    ( dmMCompare
    , SplitDiff(..)
    , LeftDiff(..)
    , RightDiff(..)
    , FurtherCompute(..)
    , DMModelDiff(..)
    , MappingDiff
    , LayerDiff
    , NodeDiff(..)
    , LinkDiff
    , LTEDiff(..)
    , TableDiff
    )
    where

import Utilities
import Types.DMModel
import Data.Validation
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as Map
import Data.Tuple (swap)



data InvalidCompare = DifferentLayerDepth
    deriving (Eq, Show)

data SplitDiff a b = SD (LeftDiff a) (RightDiff a) (FurtherCompute b)
                    deriving (Show, Eq)
data LeftDiff a = LD a deriving (Show, Eq)
data RightDiff a = RD a deriving (Show, Eq)
data FurtherCompute a = FC a deriving (Show, Eq)

data DMModelDiff
    = CoarseDiff MappingDiff LayerDiff DMModelDiff
    | FineDiff LayerDiff
    deriving (Show, Eq)

type MappingDiff = SplitDiff ModelMapping (SplitDiff ModelMapping ModelMapping)
type LayerDiff = ((ModelName, ModelName), SplitDiff [NodeName] [NodeDiff])
data NodeDiff = NodeDiff { nDiffName :: NodeName
                         , nTypeDiff :: Maybe (NodeType, NodeType)
                         , linkDiff :: LinkDiff
                         , tableDiff :: Either (NodeGate, NodeGate) TableDiff
                         } deriving (Show, Eq)
type LinkDiff = SplitDiff [(DMLink, NodeName)] [LTEDiff]
data LTEDiff = LTEDiff { inNodeName :: NodeName
                       , lTDiff :: Maybe (LinkType, LinkType)
                       , lEDiff :: Maybe (LinkEffect, LinkEffect)
                       } deriving (Show, Eq)
type TableDiff = ((DMNode, DMNode), SplitDiff TruthTable TruthTable)
                     
dmMCompare :: DMModel -> DMModel -> Validation InvalidCompare DMModelDiff
dmMCompare mL mR = case mrDepth == mlDepth of
    False -> Failure DifferentLayerDepth
    True  -> Success $ dmMCompare' mL mR
    where
        mrDepth = length $ modelLayers mR
        mlDepth = length $ modelLayers mL
        dmMCompare' (LayerBinding mM1 mL1 dM1) (LayerBinding mM2 mL2 dM2) =
            CoarseDiff (mapCompare mM1 mM2)
                       (layerCompare mL1 mL2)
                       (dmMCompare' dM1 dM2)
        dmMCompare' (Fine mL1) (Fine mL2) = FineDiff $ layerCompare mL1 mL2

mapCompare :: ModelMapping -> ModelMapping -> MappingDiff
mapCompare mL mR = let 
    lSwitchName = [x | x <- mL, notElem (fst x) (fst <$> mR)]
    rSwitchName = [x | x <- mR, notElem (fst x) (fst <$> mL)]
    fcSwitchNameL = [x | x <- mL, elem (fst x) (fst <$> mR)]
    fcSwitchNameR = [x | x <- mR, elem (fst x) (fst <$> mL)]
    lSwitchContent = fcSwitchNameL L.\\ fcSwitchNameR
    rSwitchContent = fcSwitchNameR L.\\ fcSwitchNameL
    fcSwitchContent = lSwitchContent `L.intersect` rSwitchContent
  in SD (LD lSwitchName) (RD rSwitchName) (FC (SD (LD lSwitchContent)
                                                  (RD rSwitchContent)
                                                  (FC fcSwitchContent)))

layerCompare :: ModelLayer -> ModelLayer -> LayerDiff
layerCompare mlL mlR = let
    nN = (nodeName . nodeMeta . fst)
    layerMetas = isoBimap modelMeta (mlL, mlR)
    layerGraphs = isoBimap modelGraph (mlL, mlR)
    layerNames = isoBimap modelName layerMetas
    (nodesWLinksL, nodesWLinksR) = isoBimap inAdjs layerGraphs
    lNodes = [x | x <- nodesWLinksL, notElem (nN x) (nN <$> nodesWLinksR)]
    lNodeNames = nN <$> lNodes
    rNodes = [x | x <- nodesWLinksR, notElem (nN x) (nN <$> nodesWLinksL)]
    rNodeNames = nN <$> rNodes
    fcNodesWLinksL = [x | x <- nodesWLinksL, elem (nN x) (nN <$> nodesWLinksR)]
    fcNodesWLinksR = [x | x <- nodesWLinksR, elem (nN x) (nN <$> nodesWLinksL)]
    nDiff = nodesCompare fcNodesWLinksL fcNodesWLinksR
  in (layerNames, SD (LD lNodeNames) (RD rNodeNames) (FC nDiff))

nodesCompare :: [InAdj] -> [InAdj] -> [NodeDiff]
nodesCompare inAdjsL inAdjsR = nodeCompare <$> (zip sortedL sortedR)
    where sortedL = L.sortBy nSort inAdjsL
          sortedR = L.sortBy nSort inAdjsR
          nSort x y = compare ((nodeName . nodeMeta . fst) x)
                              ((nodeName . nodeMeta . fst) y)

nodeCompare :: (InAdj, InAdj) -> NodeDiff
nodeCompare nlPair =
    NodeDiff nName typeMaybe (linkCompare links) (ttCompare nlPair)
    where
        nName = (nodeName . nodeMeta . fst) nodes
        typeMaybe | pairSame types = Nothing
                  | otherwise = Just types
        types = isoBimap (nodeType . nodeMeta) nodes
        nodes = isoBimap fst nlPair
        links = isoBimap snd nlPair

linkCompare :: ([(DMLink, NodeName)], [(DMLink, NodeName)]) -> LinkDiff
linkCompare (lLinks, rLinks) = let
    ldLinks = [x | x <- lLinks, notElem (snd x) (snd <$> rLinks)]
    rdLinks = [x | x <- rLinks, notElem (snd x) (snd <$> lLinks)]
    pComp (_, n1) (_, n2) = compare n1 n2
    lFCLinks = L.sortBy pComp [x | x <- lLinks, elem (snd x) (snd <$> rLinks)]
    rFCLinks = L.sortBy pComp [x | x <- rLinks, elem (snd x) (snd <$> lLinks)]
    fcLinks = zip lFCLinks rFCLinks
  in SD (LD ldLinks) (RD rdLinks) (FC $ lteCompare <$> fcLinks)

-- Assumes that both NodeNames are the same. 
lteCompare :: ((DMLink, NodeName), (DMLink, NodeName)) -> LTEDiff
lteCompare ((lLink, _), (rLink, rN)) = let
    ltdMaybe | linkType lLink == linkType rLink = Nothing
             | otherwise = Just (linkType lLink, linkType rLink)
    ledMaybe | linkEffect lLink == linkEffect rLink = Nothing
             | otherwise = Just (linkEffect lLink, linkEffect rLink)
  in LTEDiff rN ltdMaybe ledMaybe

ttCompare :: (InAdj, InAdj) -> Either (NodeGate, NodeGate) TableDiff
ttCompare inAdjPair = (,) nodes <$> (SD <$> (LD <$> diffLMap)
                                        <*> (RD <$> diffRMap)
                                        <*> (FC <$> sameMap))
    where
        diffRMap = Map.fromList <$> (snd <<<$>>> diff)
        diffLMap = Map.fromList <$> (fst <<<$>>> diff)
        sameMap  = Map.fromList <$> (fst <<<$>>> same)
        same = (filter (pairSame . snd)) <$> zippedLs
        diff = (filter (not . pairSame . snd)) <$> zippedLs
        zippedLs = (uncurry (zipWith (\(a, b) (_, d) -> (a, (b, d)))))
            <$> sortedLists
        sortedLists = isoBimap (L.sort . Map.toList) <$> alignedTables
        alignedTables = isoBimap (gateTable . nodeGate) <$> alignedNodes
        alignedNodes = tableAlign nodes
        nodes = isoBimap fst inAdjPair

tableAlign :: (DMNode, DMNode) -> Either (NodeGate, NodeGate) (DMNode, DMNode)
tableAlign nodes = case arePermutes lOrder rOrder of
  False -> Left $ isoBimap nodeGate nodes
  True -> Right $ const (DMNode rNM (NodeGate rNN rGO rAss nRTT rGOr)) <$> nodes
  where
    nRTT = Map.fromList $ vecReorder <$> Map.toList rTT
    vecReorder = swap . (fmap (flip U.backpermute reorderVec)) . swap
    (DMNode rNM (NodeGate rNN rGO rAss rTT rGOr)) = snd nodes
    reorderVec =
        U.fromList $ fst <$> (sortWithOrderOn snd lOrder $ zip [0..] rOrder)
    (lOrder, rOrder) = isoBimap gateOrder gates
    gates = isoBimap nodeGate nodes