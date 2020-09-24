{-# LANGUAGE OverloadedStrings #-}

module Types.GML (
      GML
    , GValue(..)
    , GKey
    , mkGKey
    , InvalidUpdate(..)
    , InvalidGML(..)
    , gmlValidate
    , gmlNodes
    , gmlNodeID
    , gmlNodeName
    , gmlEdges
    , gmlEdgeIDs
    , gmlCoords
    , gmlColor
    , gmlGraphics
    , gmlLabelGraphics
    , gmlIsGroup
    , gmlFill
    ) where

import Utilities
import Data.Validation
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified Data.Colour as CO
import qualified Data.Colour.SRGB as SCO
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.List as L

-- Define our internal representation of a Graph Modeling Language object. Note
-- that a GKey should start with an alphabetic character and only have
-- alphanumeric characters thereafter. 
type GML = [(GKey, GValue)]
data GValue = GInt Int
            | GReal Double
            | GStr T.Text
            | GList GML
            deriving (Eq, Show)
type GKey = T.Text

mkGKey :: T.Text -> Validation [GMLInvalid] GKey
mkGKey k = case (T.null k, testRs) of
    (True, _) -> Failure [EmptyKey]
    (False, (_:_)) -> Failure testRs
    (False, []) -> Success k
    where
        testRs = errorRollup $ sequenceA [ startsAlpha
                                         , bulkAlphaNum
                                         ] k


startsAlpha :: T.Text -> Validation GMLInvalid GKey
startsAlpha k
    | C.isAlpha $ T.head k = Success k
    | otherwise            = Failure $ StartWithAlpha k

bulkAlphaNum :: T.Text -> Validation GMLInvalid GKey
bulkAlphaNum k
    | T.all C.isAlphaNum k = Success k
    | otherwise            = Failure $ BulkAlphaNum k

data GMLInvalid = EmptyKey
                | StartWithAlpha T.Text
                | BulkAlphaNum T.Text

-- Possible errors when validating a parsed GML
data InvalidGML = NoGraph
                | TooManyGraphs
                | GraphsInNodes
                deriving (Eq, Show)

-- Possible errors when updating a DMModel with a parsed GML
data InvalidUpdate = MissingGMLNodes NodeSetDiff
                   | ExcessGMLNodes NodeSetDiff
                   | MissingGMLLinks LinkSetDiff
                   | ExcessGMLLinks LinkSetDiff
                   | MalformedGroups MappingDiffMap
                   deriving (Eq, Show)

type NodeSetDiff = Set.HashSet (Int, T.Text)
type LinkSetDiff = Set.HashSet (Int, Int)
type MappingDiffMap = Map.HashMap T.Text T.Text

nodeCheck :: GML -> Validation [InvalidGML] GML
nodeCheck g
    | errs == [] = Success g
    | otherwise  = Failure errs
    where
        errs        = errorRollup testResults
        testResults = [ noGraphs nodes
                      ]
        nodes = (\(_, (GList x)) -> x)
            <$> (filter (\(k, _) -> k == "node") graph)
        graph = (filter (\(k, _) -> k == "graph") g)

noGraphs :: [GML] -> Validation InvalidGML [GML]
noGraphs g
    | nGraphs   = Success g
    | otherwise = Failure GraphsInNodes
    where
        nGraphs = all (all (\(k, _) -> k /= "graph")) g

-- The GML specification is quite general. Here we check that the parsed GML
-- has the correct structure to be matched up with a parsed dmms file. 
gmlValidate :: GML -> Validation [InvalidGML] GML
gmlValidate g
    | numGraphs <  1 = Failure [NoGraph]
    | numGraphs  > 1 = Failure [TooManyGraphs]
    | otherwise      = nodeCheck g
    where
        numGraphs = length $ filter (\(x, _) -> x == "graph") g

-- The following utilities assume that a GML has been validated by gmlValidate

-- Extract the GML inside the ("graph", GList GML) pair in a parsed gml file.
gmlGraph :: GML -> GML
gmlGraph gml = let [(_, GList g)] = (filter (\(k, _) -> k == "graph") gml)
               in g


-- Utilities on GML nodes:
-- Extract the GMLs inside the ("node", GList GML) pairs in an graph gml file.
gmlNodes :: GML -> [GML]
gmlNodes gml = (\(_, (GList x)) -> x) <$> nodes
    where
        nodes = filter (\(k, _) -> k == "node") graph
        graph = gmlGraph gml

gmlNodeID :: GML -> Int
gmlNodeID gmlN = let [(_, GInt i)] = (filter (\(k, _) -> k == "id") gmlN)
                 in i

gmlNodeName :: GML -> T.Text
gmlNodeName gmlN = let [(_, GStr i)] = (filter (\(k, _) -> k == "label") gmlN)
                   in i

gmlCoords :: GML -> U.Vector Double
gmlCoords gmlN = U.fromList [x, y]
    where
        x = case head $ filter (\(k, _) -> k == "x") gmlGr of
            (_, GReal xReal) -> xReal
            (_, GInt xInt)  -> (fromIntegral xInt :: Double)
        y = case head $ filter (\(k, _) -> k == "y") gmlGr of
            (_, GReal yReal) -> yReal
            (_, GInt yInt)  -> (fromIntegral yInt :: Double)
        gmlGr = gmlGraphics gmlN

gmlColor :: GML -> CO.Colour Double
gmlColor gmlN
    | gmlIsGroup gmlN = gmlFill (gmlLabelGraphics gmlN)
    | otherwise       = gmlFill (gmlGraphics gmlN)

gmlGraphics :: GML -> GML
gmlGraphics gmlN =
    let [(_, GList i)] = (filter (\(k, _) -> k == "graphics") gmlN)
    in i

gmlLabelGraphics :: GML -> GML
gmlLabelGraphics gmlN =
    let [(_, GList i)] = (filter (\(k, _) -> k == "LabelGraphics") gmlN)
    in i

gmlIsGroup :: GML -> Bool
gmlIsGroup gmlN = case L.lookup "isGroup" gmlN of
    Nothing -> False
    Just _  -> True

-- The following extract the fill color from the Graphics or LabelGraphics in a
-- node GML
gmlFill :: GML -> CO.Colour Double
gmlFill gmlG = let [(_, GStr i)] = (filter (\(k, _) -> k == "fill") gmlG)
               in SCO.sRGB24read $ T.unpack i

-- Utilities on GML edges
-- Extract the GMLs inside the ("edge", GList GML) pairs in a parsed gml file.
gmlEdges :: GML -> [GML]
gmlEdges gml = (\(_, (GList x)) -> x) <$> edges
    where
        edges = filter (\(k, _) -> k == "edge") graph
        graph = gmlGraph gml

gmlEdgeIDs :: GML -> (Int, Int)
gmlEdgeIDs gml = (i, k)
    where
        [(_, GInt i)] = (filter (\(x, _) -> x == "source") gml)
        [(_, GInt k)] = (filter (\(y, _) -> y == "target") gml)
