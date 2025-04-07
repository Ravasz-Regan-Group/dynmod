{-# LANGUAGE OverloadedStrings #-}


module Visualize (
      toGML
    , updateDMModel
    ) where

import Utilities
import Types.GML
import Types.DMModel
import Data.Validation
import qualified Data.Text as T
import qualified Data.Colour as C
import qualified Data.Colour.SRGB as SC
import qualified Data.Vector.Unboxed as U
import qualified Data.Graph.Inductive as Gr
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Data.Bifunctor as B
import Data.Tuple

-- Pare down a DMModel into a GML, with the Fine ModelLayer as the actual nodes,
-- and succesive layers on top as groups, and groups of groups, etc...
toGML :: DMModel -> GML
toGML (Fine mll) = fileHeader <> bulk
    where
        bulk = [("graph", GList $ graphHeader <> (gNodes <> gEdges))]
        gEdges = mkGMLEdge <$> es
        gNodes = (mkGMLNode Map.empty False) <$> ns
        (ns, es) = (Gr.labNodes graph, Gr.labEdges graph)
        graph = modelGraph mll
        fileHeader = [ ("Creator", GStr "yFiles")
                     , ("Version", GStr "2.17")
                     ]
        graphHeader = [ ("hierarchic", GInt 1)
                      , ("label", GStr "")
                      , ("directed", GInt 1)
                      ]
toGML (LayerBinding mm ml dm) = fileHeader <> bulk
    where
        bulk = [("graph", GList $ graphHeader
                <> (rearrange $ go mm ml dm Map.empty))]
        rearrange = concatPair . B.bimap concat concat . unzip
        go :: ModelMapping
           -> ModelLayer
           -> DMModel
           -> Map.HashMap Int Int
           -> [([(GKey, GValue)], [(GKey, GValue)])]
        go mapping layer (LayerBinding mm' ml' dm') groupIDs =
            (gNodes, gEdges) : (go mm' ml' dm' newGroupIDs)
                where
                    gEdges = mkGMLEdge <$> es
                    gNodes = (mkGMLNode groupIDs True) <$> ns
                    newGroupIDs = mkGroupIDs ((fst . modelMappingSplit) mapping)
                                             ns
                                             fineNS
                    fineNS = (Gr.labNodes . modelGraph) ml'
                    ns = Gr.labNodes graph
                    es = Gr.labEdges graph
                    graph = modelGraph layer
        go mapping layer (Fine ml') groupIDs = [ (gNodes, gEdges)
                                               , (fineGMLNodes, fineGMLEdges)]
            where
                fineGMLEdges = mkGMLEdge <$> fineES
                fineGMLNodes = (mkGMLNode fineGroupIDs False) <$> fineNS
                fineGroupIDs = mkGroupIDs ((fst . modelMappingSplit) mapping)
                                          ns
                                          fineNS
                fineNS = (Gr.labNodes . modelGraph) ml'
                fineES = (Gr.labEdges . modelGraph) ml'
                gEdges = mkGMLEdge <$> es
                gNodes = (mkGMLNode groupIDs True) <$> ns
                ns = Gr.labNodes graph
                es = Gr.labEdges graph
                graph = modelGraph layer
        fileHeader = [ ("Creator", GStr "yFiles")
                     , ("Version", GStr "2.17")
                     ]
        graphHeader = [ ("hierarchic", GInt 1)
                      , ("label", GStr "")
                      , ("directed", GInt 1)
                      ]

-- Make a GML (GKey, GValue) pair out of a Gr.LNode DMNode. There are additional
-- fields if the node in question is part of a group or is a group, so we pass
-- that information in as well. 
mkGMLNode :: Map.HashMap Int Int -> Bool -> Gr.LNode DMNode -> (GKey, GValue)
mkGMLNode groupIDs isGroup (nodeID, dmNode)
    | isGroup = ((mkGKey "node") `orElse` "error", gValue)
    | otherwise = ((mkGKey "node") `orElse` "error", fValue)
    where
        gValue = case Map.lookup nodeID groupIDs of
                Nothing -> GList groupList
                Just gID -> GList $ groupList
                    <> [((mkGKey "gid") `orElse` "error", GInt $ gID)]
        fValue = case Map.lookup nodeID groupIDs of
                Nothing -> GList fineList
                Just gID -> GList $ fineList
                    <> [((mkGKey "gid") `orElse` "error", GInt $ gID)]
        groupList = [ nID
                    , nLabel
                    , ((mkGKey "graphics") `orElse` "error", GList gGraphics)
                    , ((mkGKey "LabelGraphics") `orElse` "error"
                        , GList gLabelGraphics)
                    , yesGroup
                    ]
        nID = ((mkGKey "id") `orElse` "error", GInt $ nodeID)
        nLabel = ((mkGKey "label") `orElse` "error", GStr nName)
        gGraphics = [ xVal
                    , yVal
                    , noFill
                    ]
        noFill = ((mkGKey "hasFill") `orElse` "error", GInt 0)
        gLabelGraphics = [ gFontSize
                         , gFontStyle
                         , anchor
                         , nFill
                         ]
        fineList = [ nID
                   , nLabel
                   , ((mkGKey "graphics") `orElse` "error", GList fGraphics)
                   , ((mkGKey "LabelGraphics") `orElse` "error"
                        , GList fLabelGraphics)
                   ]
        fGraphics = [ xVal
                    , yVal
                    , width
                    , height
                    , nFill
                    ]
        width = ((mkGKey "w") `orElse` "error", GReal $ nWidth nName)
        nWidth xs = max 30 $ (((fromIntegral . T.length) xs)*30)/3.5
        height = ((mkGKey "h") `orElse` "error", GInt 30)
        fLabelGraphics = [ fFontSize
                         ]
        xVal = ((mkGKey "x") `orElse` "error" , GReal $ coords U.! 0)
        yVal = ((mkGKey "y") `orElse` "error" , GReal $ coords U.! 1)
        gFontSize = ((mkGKey "fontSize") `orElse` "error", GInt 14)
        fFontSize = ((mkGKey "fontSize") `orElse` "error", GInt 12)
        gFontStyle = ((mkGKey "fontStyle") `orElse` "error", GStr "bold")
        anchor = ((mkGKey "anchor") `orElse` "error", GStr "tr")
        nFill = ((mkGKey "fill") `orElse` "error", GStr nColor)
        yesGroup = ((mkGKey "isGroup") `orElse` "error", GInt 1)
        nColor = (T.pack . SC.sRGB24show . nodeColor . nodeMeta) dmNode
        coords = (nodeCoordinate . nodeMeta) dmNode
        nName = (nodeName . nodeMeta) dmNode
        
-- Make a GML (GKey, GValue) pair out of a Gr.LEdge DMLink.
mkGMLEdge :: Gr.LEdge DMLink -> (GKey, GValue)
mkGMLEdge l = ((mkGKey "edge") `orElse` "error", lValue)
  where
    lValue = GList $ [ source
                     , target
                     , graphics
                     ]
      where
        source = ((mkGKey "source") `orElse` "error", GInt $ fstOf3 l)
        target = ((mkGKey "target") `orElse` "error", GInt $ sndOf3 l)
        graphics = ((mkGKey "graphics") `orElse` "error", graphicsVal)
        graphicsVal = GList $ [ eFill
                              , arrowType
                              ]
          where
            eFill = ("fill", (GStr . T.pack . SC.sRGB24show)
                (C.black :: C.Colour Double))
            arrowType = gmlArrowType $ linkEffect dmLink
    dmLink = thdOf3 l

gmlArrowType :: LinkEffect -> (GKey, GValue)
gmlArrowType Undefined_LE =
    ((mkGKey "targetArrow") `orElse` "error", GStr "skewed_dash")
gmlArrowType Activation =
    ((mkGKey "targetArrow") `orElse` "error", GStr "standard")
gmlArrowType Repression =
    ((mkGKey "targetArrow") `orElse` "error", GStr "t_shape")
gmlArrowType Context_Dependent =
    ((mkGKey "targetArrow") `orElse` "error", GStr "concave")
gmlArrowType Inapt =
    ((mkGKey "targetArrow") `orElse` "error", GStr "diamond")

-- Create a HashMap Int Int that maps from a given nId to its corresponding gID.
-- That it, match the fgl Node of a given DMNode to the fgl Node of the module
-- it belongs to. This assumes that the fgl Nodes all come from a single parsed
-- DMMS file, and so have unique Ints. 
mkGroupIDs :: DMMSModelMapping
           -> [Gr.LNode DMNode] -- | The coarse layer
           -> [Gr.LNode DMNode] -- | The fine layer
           -> Map.HashMap Gr.Node Gr.Node
mkGroupIDs mapping cns fns = Map.fromList combos
    where
        combos = (B.bimap intFind intFind) <$> nameCombos
        intFind = (Map.!) nameMap
        nameMap = Map.fromList $ swap <$> nameIDs
        nameIDs = (nodeName . nodeMeta) <<$>> (cns <> fns)
        nameCombos = invertModelMapping mapping

invertModelMapping :: DMMSModelMapping -> [(NodeName, NodeName)]
invertModelMapping ns = concatMap flipper ns
    where
        flipper :: (a, [b]) -> [(b, a)]
        flipper (nName, nNames) = ((flip (,)) nName) <$> nNames

-- Make a map whose keys are node IDs, and whose values are the node IDs of the
-- ModelMapping switch that they belong to. 
-- mkDMMGroupIDs :: DMModel -> Map.HashMap Gr.Node Gr.Node
-- mkDMMGroupIDs (Fine _) = Map.empty
-- mkDMMGroupIDs (LayerBinding mm ml dmm) =
--     gIDMap `Map.union` (mkDMMGroupIDs dmm)
--     where
--         gIDMap = mkGroupIDs mm cns fns
--         fns = (Gr.labNodes . modelGraph . coarseLayer) dmm
--         cns = (Gr.labNodes . modelGraph) ml

-- Make a map whose keys are NodeNames, and whose values are the
-- Gr.LNode DMNodes of the ModelMapping switch that they belong to. 
mkDMMGroupNS :: DMModel -> Map.HashMap NodeName (Gr.LNode DMNode)
mkDMMGroupNS (Fine _) = Map.empty
mkDMMGroupNS (LayerBinding mm ml dmm) = gNodeMap `Map.union` (mkDMMGroupNS dmm)
    where
        gNodeMap = Map.fromList $ nodesGet fGraph cGraph <$> gIDList
        nodesGet :: Gr.Gr DMNode DMLink -> Gr.Gr DMNode DMLink -> (Int, Int)
                 -> (NodeName, Gr.LNode DMNode) 
        nodesGet fg cg (i, j) =
            ((nodeName . nodeMeta . fromJust . Gr.lab fg) i
                , (j, (fromJust . Gr.lab cg) j))
        gIDList = Map.toList $ mkGroupIDs ((fst . modelMappingSplit) mm) cns fns
        fns = Gr.labNodes fGraph
        fGraph = (modelGraph . coarseLayer) dmm
        cns = Gr.labNodes cGraph
        cGraph = modelGraph ml
        

-- Update a DMModel with data from a GML object. Typically node position & color
-- , but that may change with time. This presumes that the passed GML has
-- already been validated by gmlValidate.
updateDMModel :: DMModel -> GML -> Validation [InvalidUpdate] DMModel
updateDMModel d g = (unsafeUpDDMM tMap) <$> (d `areIso` g)
    where tMap = Map.fromList $ gNodeCondition <$> (gmlNodes g)

unsafeUpDDMM :: GmlDMNodeTransMap -> DMModel -> DMModel
unsafeUpDDMM tMap (Fine (ModelLayer mg mm)) =
    Fine (ModelLayer (Gr.nmap (updateDMNode tMap) mg) mm)
unsafeUpDDMM tMap (LayerBinding mm (ModelLayer mg mMeta) dmM) =
    LayerBinding  mm
                 (ModelLayer (Gr.nmap (updateDMNode tMap) mg) mMeta)
                 (unsafeUpDDMM tMap dmM)
    

-- Update a DMNode. Typically node position & color, but that may change with
-- time. This presumes that the passed GML has already been validated by
-- gmlValidate.
updateDMNode :: GmlDMNodeTransMap -> DMNode -> DMNode
updateDMNode tMap (DMNode nMeta dmNG) =
    (DMNode (nMeta {nodeColor = newColor, nodeCoordinate = newCoords}) dmNG)
    where
        newColor = tColor transfer
        newCoords = tCoords transfer
        transfer = tMap Map.! (nodeName nMeta)

-- Pull from a GML node the data we need to update a DMNode.This may change over
-- time. This presumes that the passed GML has already been validated by
-- gmlValidate.
gNodeCondition :: GML -> (NodeName, GmlDMNodeTrans)
gNodeCondition gmlN = (gName, GmlDMNodeTrans nColor gCoords)
    where
        gName = gmlNodeName gmlN
        gCoords = gmlCoords gmlN
        nColor = gmlColor gmlN

type GmlDMNodeTransMap = Map.HashMap NodeName GmlDMNodeTrans
data GmlDMNodeTrans = GmlDMNodeTrans { tColor :: LocalColor
                                     , tCoords :: U.Vector Double
                                     } deriving (Show, Eq)

-- This presumes that the passed GML has already been validated by gmlValidate.
areIso :: DMModel -> GML -> Validation [InvalidUpdate] DMModel
areIso dmm gml
    | errs /= [] = Failure errs
    | otherwise = case gmlGroupsMatch dmm gml of
        Success (d, _) -> Success d
        Failure y -> Failure [y]
            
    where
        errs = errorRollup testResults
        testResults = [ gmlNSMatch dmm gml
                      , gmlESMatch dmm gml
                      ]

-- Make sure that the node id & labels of the GML graph match up with the node
-- id and NodeName of the DMModel. 
gmlNSMatch :: DMModel -> GML -> Validation InvalidUpdate (DMModel, GML)
gmlNSMatch dmm gml
    | gNSIDNameSet == dNSIDNameSet = Success (dmm, gml)
    | eDiff == Set.empty           = Failure $ MissingGMLNodes mDiff
    | otherwise                    = Failure $ ExcessGMLNodes eDiff
    where
        eDiff = Set.difference gNSIDNameSet dNSIDNameSet
        mDiff = Set.difference dNSIDNameSet gNSIDNameSet
        dNSIDNameSet = Set.fromList $
            (\(k, dn) -> (k, (nodeName . nodeMeta) dn)) <$> dNS
        dNS = concat $ modelNodes' dmm
        gNSIDNameSet = Set.fromList $
            (\(i, gn) -> (i, gmlNodeName gn)) <$> gNS
        gNS = gmlNodeMapPrep $ gmlNodes gml
        gmlNodeMapPrep ns = zip (gmlNodeID <$> ns) ns

-- Make sure that the source & target IDs of the GML graph match up with the
-- source & target IDs of the DMModel. 
gmlESMatch :: DMModel -> GML -> Validation InvalidUpdate (DMModel, GML)
gmlESMatch dmm gml
    | gEdgeSet == dEdgeSet = Success (dmm, gml)
    | eDiff == Set.empty           = Failure $ MissingGMLLinks mDiff
    | otherwise                    = Failure $ ExcessGMLLinks eDiff
    where
        eDiff = Set.difference gEdgeSet dEdgeSet
        mDiff = Set.difference dEdgeSet gEdgeSet
        dEdgeSet :: Set.HashSet (Int, Int)
        dEdgeSet = Set.fromList $ edgeMapPrep <$> (concat $ modelEdges' dmm)
        edgeMapPrep (i, j, _) = (i, j)
        gEdgeSet :: Set.HashSet (Int, Int)
        gEdgeSet = Set.fromList $ gmlEdgeIDs <$> (gmlEdges gml)

-- Make sure that the group structure of the GML graph match up with the
-- ModelMappings of the DMModel. 
gmlGroupsMatch :: DMModel -> GML -> Validation InvalidUpdate (DMModel, GML)
gmlGroupsMatch dmm gml
    | diffGroups == Map.empty = Success (dmm, gml)
    | otherwise = Failure $ MalformedGroups diffGroups
    where
        diffGroups = differenceWithKey groupCheck gGMap dGMap
        groupCheck k n1 n2
            | n1 == n2 = Nothing
            | otherwise = Just (k <> ", " <> n1 <> ", " <>  n2)
        dGMap = Map.map (nodeName . nodeMeta . snd) $ mkDMMGroupNS dmm
        gGMap = Map.fromList $ gmlNames gNodes <$> gFNIDGID
        gmlNames gns (m, n) = ((fromJust . findGMLName gns) m
                                , (fromJust . findGMLName gns) n)
--      (ID, GID) of all fine nodes
        gFNIDGID = idgidGet <$> gFineNodes
        idgidGet gn = (fromJust idInt, fromJust gidInt)
            where
                idInt = case snd idPair of
                    GInt i -> Just i
                    _ -> Nothing
                idPair = head $ filter (\(k, _) -> k == "id") gn
                gidInt = case snd gidPair of
                    GInt j -> Just j
                    _ -> Nothing
                gidPair = head $ filter (\(k, _) -> k == "gid") gn
--      All the GML nodes that have a GID
        gFineNodes = filter hasGID gNodes
        hasGID n = case L.lookup "gid" n of
            Nothing -> False
            Just _  -> True
        gNodes = gmlNodes gml

-- Find the label of the given id from a list of GML nodes. 
findGMLName :: [GML] -> Int -> Maybe NodeName
findGMLName gns i = gmlNodeName <$> (findGMLNode gns i)

-- Find the node of the given id from a list of GML nodes. This assumes that
-- there are no identical IDs, which would cause yEd to crash in any case. 
findGMLNode :: [GML] -> Int -> Maybe GML
findGMLNode gns i = case rs of
    [] -> Nothing
    _  -> Just (head rs)
    where
        rs = filter (\x -> gmlNodeID x == i) gns
