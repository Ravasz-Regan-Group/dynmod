module SBML
    (dmModelToSBML)
    where

import Types.DMModel
import Types.Simulation
import Text.XML.HXT.Core
import qualified Data.Text as T
import qualified Data.Bimap as BM
import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import Data.Maybe (fromJust)

-- Export a DMModel to SBML. For now, only export the Fine ModelLayer. 
dmModelToSBML :: ArrowXml a => DMModel -> a XmlTree XmlTree
dmModelToSBML dmModel = mkelem "sbml"
    [ sqattr xmlnsQN "http://www.sbml.org/sbml/level3/version1/core"
    , sattr "level" "3"
    , sattr "version" "1", txt "\n     "
    , sqattr notesQN $ namespaceUri notesQN, txt "\n     "
    , sqattr qualQN $ namespaceUri qualQN
    , sattr "qual:required" "true"
    ]
    [ mkelem "model"
        [ sattr "id" mNamestr
        , sattr "sboTerm" "SBO:0000548" -- multi-valued logical framework
        ]
        [ selem "notes"
            [selem "xhtml:body"
                [selem "xhtml:p"
                    [(txt . T.unpack . desc . modelInfo) bLModelMeta]
                ]
            ]
        , selem "listOfCompartments"
            [aelem "compartment"
                [ sattr "id" "FineLayer"
                , sattr "constant" "true"
                , sattr "sboTerm" "SBO:0000410" -- implicit compartment
                ]
            ]
        , mkelem "qual:listOfQualitativeSpecies"
            [sqattr qualQN $ namespaceUri qualQN]
            nodeSpecies
        , mkelem "qual:listOfTransitions"
            [sqattr qualQN $ namespaceUri qualQN]
            nodeTransitions
        ]
    ]
    where
        (nodeSpecies, nodeTransitions) = unzip $
            dmNodeToSBML lRanges lniBMap <$> nodesWLinks
        nodesWLinks = (inAdjs . modelGraph) baseLayer
        mNamestr = (T.unpack . modelName) bLModelMeta
        lRanges = layerRanges baseLayer
        LayerSpecs lniBMap _ _ _ = layerPrep baseLayer
        bLModelMeta = modelMeta baseLayer
        baseLayer = (last . modelLayers) dmModel
        notesQN = mkQName "xmlns" "xhtml" "http://www.w3.org/1999/xhtml"
        qualQN = mkQName "xmlns" "qual"
            "http://www.sbml.org/sbml/level3/version1/qual/version1"

dmNodeToSBML :: ArrowXml a
             => LayerRange
             -> LayerNameIndexBimap
             -> InAdj
             -> (a XmlTree XmlTree, a XmlTree XmlTree)
dmNodeToSBML lRanges lniBMap (dmNode, inLinks) = (nodeSpecies, nodeTransition)
    where
        nodeTransition =
            dmNodeTransition lRanges lniBMap (dmNode, inLinks)
        nodeSpecies = mkelem "qual:qualitativeSpecies"
            [ sattr "qual:id" ("node_" ++ (show . (lniBMap BM.!)) nName)
            , sattr "qual:name" (T.unpack nName)
            , compartmentSpec, qualSpecVar
            , sattr "qual:maxLevel" nTopStr
            ]
            [selem "notes"
                [selem "xhtml:body"
                    [selem "xhtml:p"
                        [(txt . T.unpack . desc . nodeInfo) nMeta]
                    ]
                ]
            ]
        nTopStr = (show . maximum . fmap fst . gateAssigns . nodeGate) dmNode
        nName = nodeName nMeta
        nMeta = nodeMeta dmNode
        qualSpecVar = sattr "qual:constant" "false"
        compartmentSpec = sattr "qual:compartment" "FineLayer"

dmNodeTransition :: ArrowXml a
                 => LayerRange
                 -> LayerNameIndexBimap
                 -> InAdj
                 -> a XmlTree XmlTree
dmNodeTransition lRanges lniBMap (dmNode, inLinks) =
    mkelem "qual:transition"
    [ sattr "qual:id" ("node_" ++ nIndexStr ++ "_transition")
    , sattr "qual:name" (T.unpack nName ++ "_transition")
    ]
    [ selem "qual:listOfInputs" inlinkTrees
    , selem "qual:listOfOutputs"
        [aelem "qual:output"
            [ sattr "qual:id" ("node_" ++ nIndexStr ++ "_output")
            , sattr "qual:name" (T.unpack nName ++ "_output")
            , sattr "qual:qualitativeSpecies" ("node_" ++ nIndexStr)
            , sattr "qual:transitionEffect" "assignmentLevel"
            ]
        ]
    , selem "qual:listOfFunctionTerms" funtionTerms
    ]
    where
        funtionTerms
            | length gAssigns <= 2 =
                [gateAssignToSBML lRanges trueAssign, defaultTE]
            | otherwise = gateAssignToSBML lRanges <$> gAssigns
        defaultTE :: ArrowXml a => a XmlTree XmlTree
        defaultTE = aelem "qual:defaultTerm" [sattr "qual:resultLevel" "0"]
        trueAssign = (fromJust . L.find trueF) gAssigns
        trueF (gState, _) = gState == 1
        gAssigns = (gateAssigns . nodeGate) dmNode
        inlinkTrees = inLinkToSBML lniBMap nName <$> inLinks
        nIndexStr = show $ lniBMap BM.! nName
        nName = (nodeName . nodeMeta) dmNode

inLinkToSBML :: ArrowXml a
             => LayerNameIndexBimap
             -> NodeName
             -> (DMLink, NodeName)
             -> a XmlTree XmlTree
inLinkToSBML lniBMap nName (dmLink, linkName) = mkelem "qual:input"
    [ sattr "qual:id" ("node_" ++ nIndexStr ++ "_input_" ++ lINdexStr)
    , sattr "qual:name" (T.unpack nName ++ "_input_" ++ T.unpack linkName)
    , sattr "qual:qualitativeSpecies" ("node_" ++ nIndexStr)
    , sattr "qual:transitionEffect" "none"
    , sattr "qual:sign" ((linkEffectStr . linkEffect) dmLink)
    ]
    [selem "notes"
        [selem "xhtml:body"
            [selem "xhtml:p"
                [(txt . T.unpack . desc . linkInfo) dmLink]
            ]
        ]
    ]
    where
        nIndexStr = show $ lniBMap BM.! nName
        lINdexStr = show $ lniBMap BM.! linkName

linkEffectStr :: LinkEffect -> String
linkEffectStr Activation = "positive"
linkEffectStr Repression = "negative"
linkEffectStr Context_Dependent = "dual"
linkEffectStr Inapt = "unknown"
linkEffectStr Undefined_LE = "unknown"

gateAssignToSBML :: ArrowXml a
                 => LayerRange
                 -> NodeStateAssign
                 -> a XmlTree XmlTree
gateAssignToSBML lRanges (nState, nExpr) = mkelem "qual:functionTerm"
    [sattr "qual:resultLevel" (show nState)]
    [mkelem "math"
        [sqattr xmlnsQN "http://www.w3.org/1998/Math/MathML"]
        [nodeExprToMathML lRanges nExpr]
    ]

nodeExprToMathML :: ArrowXml a => LayerRange -> NodeExpr -> a XmlTree XmlTree
nodeExprToMathML lRanges nExpr = case nExpr of
    GateLit bl
        | bl -> selem "ci" [txt "TRUE"]
        | otherwise -> selem "ci" [txt "FALSE"]
    GateConst nName nState
        | lRanges M.! nName == 1 -> selem "ci" [(txt . T.unpack) nName]
        | otherwise -> selem "msub"
            [ selem "ci" [(txt . T.unpack) nName]
            , selem "cn" [(txt . show) nState]
            ]
    Not notExpr -> selem "apply" [eelem "not", nodeExprToMathML lRanges notExpr]
    Binary binOp nExpr1 nExpr2 -> selem "apply"
        [mlOp, nodeExprToMathML lRanges nExpr1, nodeExprToMathML lRanges nExpr2]
        where
            mlOp = case binOp of
                And -> eelem "and"
                Or -> eelem "or"
    Pars pExpr -> nodeExprToMathML lRanges pExpr


-- modelLayerToSBML :: ArrowXml a => ModelLayer -> a XmlTree XmlTree
-- modelLayerToSBML mL = 
--     where
--         lNodes = layerNodes mL

