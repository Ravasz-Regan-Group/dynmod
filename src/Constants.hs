{-# LANGUAGE OverloadedStrings #-}

module Constants ( svgColors
                 , svgLocalColors
                 , rws
                 , nTDesc
                 , lTDesc
                 , lEDesc
                 ) where

import Types.DMModel
import qualified Data.Text as T
import qualified Data.Colour as C
import qualified Data.Colour.Names as CN


-- SVG Color Names:
svgColors :: [T.Text]
svgColors = [
   "aliceblue"
 , "antiquewhite"
 , "aqua"
 , "aquamarine"
 , "azure"
 , "beige"
 , "bisque"
 , "black"
 , "blanchedalmond"
 , "blue"
 , "blueviolet"
 , "brown"
 , "burlywood"
 , "cadetblue"
 , "chartreuse"
 , "chocolate"
 , "coral"
 , "cornflowerblue"
 , "cornsilk"
 , "crimson"
 , "cyan"
 , "darkblue"
 , "darkcyan"
 , "darkgoldenrod"
 , "darkgray"
 , "darkgreen"
 , "darkgrey"
 , "darkkhaki"
 , "darkmagenta"
 , "darkolivegreen"
 , "darkorange"
 , "darkorchid"
 , "darkred"
 , "darksalmon"
 , "darkseagreen"
 , "darkslateblue"
 , "darkslategray"
 , "darkslategrey"
 , "darkturquoise"
 , "darkviolet"
 , "deeppink"
 , "deepskyblue"
 , "dimgray"
 , "dimgrey"
 , "dodgerblue"
 , "firebrick"
 , "floralwhite"
 , "forestgreen"
 , "fuchsia"
 , "gainsboro"
 , "ghostwhite"
 , "gold"
 , "goldenrod"
 , "gray"
 , "grey"
 , "green"
 , "greenyellow"
 , "honeydew"
 , "hotpink"
 , "indianred"
 , "indigo"
 , "ivory"
 , "khaki"
 , "lavender"
 , "lavenderblush"
 , "lawngreen"
 , "lemonchiffon"
 , "lightblue"
 , "lightcoral"
 , "lightcyan"
 , "lightgoldenrodyellow"
 , "lightgray"
 , "lightgreen"
 , "lightgrey"
 , "lightpink"
 , "lightsalmon"
 , "lightseagreen"
 , "lightskyblue"
 , "lightslategray"
 , "lightslategrey"
 , "lightsteelblue"
 , "lightyellow"
 , "lime"
 , "limegreen"
 , "linen"
 , "magenta"
 , "maroon"
 , "mediumaquamarine"
 , "mediumblue"
 , "mediumorchid"
 , "mediumpurple"
 , "mediumseagreen"
 , "mediumslateblue"
 , "mediumspringgreen"
 , "mediumturquoise"
 , "mediumvioletred"
 , "midnightblue"
 , "mintcream"
 , "mistyrose"
 , "moccasin"
 , "navajowhite"
 , "navy"
 , "oldlace"
 , "olive"
 , "olivedrab"
 , "orange"
 , "orangered"
 , "orchid"
 , "palegoldenrod"
 , "palegreen"
 , "paleturquoise"
 , "palevioletred"
 , "papayawhip"
 , "peachpuff"
 , "peru"
 , "pink"
 , "plum"
 , "powderblue"
 , "purple"
 , "red"
 , "rosybrown"
 , "royalblue"
 , "saddlebrown"
 , "salmon"
 , "sandybrown"
 , "seagreen"
 , "seashell"
 , "sienna"
 , "silver"
 , "skyblue"
 , "slateblue"
 , "slategray"
 , "slategrey"
 , "snow"
 , "springgreen"
 , "steelblue"
 , "tan"
 , "teal"
 , "thistle"
 , "tomato"
 , "turquoise"
 , "violet"
 , "wheat"
 , "white"
 , "whitesmoke"
 , "yellow"
 , "yellowgreen"
 ]

-- SVG Colors:
svgLocalColors :: [C.Colour Double]
svgLocalColors = [
   CN.aliceblue
 , CN.antiquewhite
 , CN.aqua
 , CN.aquamarine
 , CN.azure
 , CN.beige
 , CN.bisque
 , CN.black
 , CN.blanchedalmond
 , CN.blue
 , CN.blueviolet
 , CN.brown
 , CN.burlywood
 , CN.cadetblue
 , CN.chartreuse
 , CN.chocolate
 , CN.coral
 , CN.cornflowerblue
 , CN.cornsilk
 , CN.crimson
 , CN.cyan
 , CN.darkblue
 , CN.darkcyan
 , CN.darkgoldenrod
 , CN.darkgray
 , CN.darkgreen
 , CN.darkgrey
 , CN.darkkhaki
 , CN.darkmagenta
 , CN.darkolivegreen
 , CN.darkorange
 , CN.darkorchid
 , CN.darkred
 , CN.darksalmon
 , CN.darkseagreen
 , CN.darkslateblue
 , CN.darkslategray
 , CN.darkslategrey
 , CN.darkturquoise
 , CN.darkviolet
 , CN.deeppink
 , CN.deepskyblue
 , CN.dimgray
 , CN.dimgrey
 , CN.dodgerblue
 , CN.firebrick
 , CN.floralwhite
 , CN.forestgreen
 , CN.fuchsia
 , CN.gainsboro
 , CN.ghostwhite
 , CN.gold
 , CN.goldenrod
 , CN.gray
 , CN.grey
 , CN.green
 , CN.greenyellow
 , CN.honeydew
 , CN.hotpink
 , CN.indianred
 , CN.indigo
 , CN.ivory
 , CN.khaki
 , CN.lavender
 , CN.lavenderblush
 , CN.lawngreen
 , CN.lemonchiffon
 , CN.lightblue
 , CN.lightcoral
 , CN.lightcyan
 , CN.lightgoldenrodyellow
 , CN.lightgray
 , CN.lightgreen
 , CN.lightgrey
 , CN.lightpink
 , CN.lightsalmon
 , CN.lightseagreen
 , CN.lightskyblue
 , CN.lightslategray
 , CN.lightslategrey
 , CN.lightsteelblue
 , CN.lightyellow
 , CN.lime
 , CN.limegreen
 , CN.linen
 , CN.magenta
 , CN.maroon
 , CN.mediumaquamarine
 , CN.mediumblue
 , CN.mediumorchid
 , CN.mediumpurple
 , CN.mediumseagreen
 , CN.mediumslateblue
 , CN.mediumspringgreen
 , CN.mediumturquoise
 , CN.mediumvioletred
 , CN.midnightblue
 , CN.mintcream
 , CN.mistyrose
 , CN.moccasin
 , CN.navajowhite
 , CN.navy
 , CN.oldlace
 , CN.olive
 , CN.olivedrab
 , CN.orange
 , CN.orangered
 , CN.orchid
 , CN.palegoldenrod
 , CN.palegreen
 , CN.paleturquoise
 , CN.palevioletred
 , CN.papayawhip
 , CN.peachpuff
 , CN.peru
 , CN.pink
 , CN.plum
 , CN.powderblue
 , CN.purple
 , CN.red
 , CN.rosybrown
 , CN.royalblue
 , CN.saddlebrown
 , CN.salmon
 , CN.sandybrown
 , CN.seagreen
 , CN.seashell
 , CN.sienna
 , CN.silver
 , CN.skyblue
 , CN.slateblue
 , CN.slategray
 , CN.slategrey
 , CN.snow
 , CN.springgreen
 , CN.steelblue
 , CN.tan
 , CN.teal
 , CN.thistle
 , CN.tomato
 , CN.turquoise
 , CN.violet
 , CN.wheat
 , CN.white
 , CN.whitesmoke
 , CN.yellow
 , CN.yellowgreen
 ]

rws :: [T.Text]
rws = [ "ModelName"
    , "FormatVersion"
    , "ModelVersion"
    , "ModelPaper"
    , "ModelDescription"
    , "ModelNotes"
    , "Switch"
    , "BiasOrderFirst"
    , "BiasOrderLast"
    , "NodeName"
    , "NodeType"
    , "NodeColor"
    , "NodeCoordinate"
    , "NodeGenes"
    , "NodeDescription"
    , "NodeNotes"
    , "InputNode"
    , "LinkEffect"
    , "LinkType"
    , "LinkDescription"
    , "LinkNotes"
    , "BibTeXKey"
    , "ReferenceNotes"
    , "cite"
    , "textbf"
    , "textit"
    , "Model"
    , "ModelMapping"
    , "ModelGraph"
    , "ModelMetaData"
    , "Node"
    , "InLink"
    , "NodeGate"
    , "DiscreteLogic"
    , "TruthTable"
    , "NodeMetaData"
    , "CitationDictionary"
    , "BibTeXRecord"
    ]

-- Official descriptions of the various NodeTypes
nTDesc :: NodeType -> T.Text
nTDesc Undefined_NT = "Undefined Node Type"
nTDesc Cell = "Node type used when a node's state is used to represent discrete phenotypes of an entire cell; appropriate for multi models."
nTDesc DM_Switch = "Multi-stable regulatory module (Dynamically Modular Switch) in a higher-level model layer."
nTDesc Connector = "Grouping of nodes that are either mono-stable, or do not form a switch-like circuit that controls discrete phenotype transitions. For example, signaling input layers without feedback, or multi-step links between distinct switches can be represented as connector nodes in coarse-grained (switch-level) models."
nTDesc Environment = "Nodes or modules that represent the extracellular environment of a single cell or cell collective. These are self-sustaining nodes or node groups that maintain their initial states and receive no feedback from the rest of the network (they act as inputs)."
nTDesc Process = "Nodes or modules that stand in for complex cellular processes not modeled in detail (e.g., DNA replication or the process of aligning chromosomes at the metaphase plane during mitosis)."
nTDesc Macro_Structure = "Nodes or modules that represent the state of large, complex cellular structures such as DNA content, cytoskeletal features, junctions or mitochondria."
nTDesc Metabolite = "Regulatory node representing a metabolite (not protein, gene product or complex structure)."
nTDesc MRNA = "mRNA."
nTDesc MicroRNA = "microRNA."
nTDesc Protein_Complex = "Protein complex represented by a single node or via a key member of the complex."
nTDesc Receptor = "Cell surface receptor protein or complex."
nTDesc Adaptor_Protein = "Protein that helps scaffold a signaling complex or other large assembly of proteins."
nTDesc Secreted_Protein = "Protein secreted into the extracellular environment, such that the state of the node tagged with this type represents the availability fo this protein outside the cell."
nTDesc TF_Protein = "Transcription factor."
nTDesc Kinase = "Kinase (enzyme that catalyzes the phosphorylation of its target)."
nTDesc Phosphatase = "Phosphatase (enzyme that catalyzes the removal of phosphorylation from its target)."
nTDesc Ubiquitin_Ligase = "Ubiquitin ligase (protein that recruits an ubiquitin-conjugating enzyme that has been loaded with ubiquitin to a target protein and assists or directly catalyzes the transfer of ubiquitin from the ubiquitin-conjugating enzyme to the target)."
nTDesc Protease = "Protease (enzyme that catalyzes the breakdown of proteins into smaller fragments)."
nTDesc DNase = "Protease ligase."
nTDesc CAM = "Cell adhesion proteins located on the cell surface."
nTDesc CDK = "Cyclin-dependent kinase."
nTDesc CDKI = "Cyclin-dependent kinase inhibitor."
nTDesc GEF = "Guanine nucleotide exchange factor."
nTDesc GAP = "GTPase-activating protein (also called GTPase-accelerating protein)."
nTDesc GTPase = "GTPase enzymes that hydrolyze ATP to ADP."
nTDesc Enzyme = "Enzyme that does not fit the more specific enzyme categories listed above."
nTDesc Protein = "Regulatory protein that does not fit any of the more specific classifications listed above."
nTDesc Membrane_Potential = "A relative measure of membrane potential across a biological membrane, generally indicating whether this potential is within the  normal range, or abnormally low / high in a way that affects other regulatory processes."
nTDesc LncRNA = "Long intervening noncoding RNA"
nTDesc Cell_Surgace_Ligand = "Membrane-bound signaling molecule that serves as a ligand to receptors on neighboring cells."

-- Official descriptions of the various LinkTypes
lTDesc :: LinkType -> T.Text
lTDesc Undefined_LT = "Undefined Link Type"
lTDesc Enforced_Env = "This link type represents self-loops on Environment nodes, which guarantee that these nodes maintain their initial state throughout a time-course simulation unless they are explicitly altered by the simulation’s settings."
lTDesc Indirect = "Regulatory influence that does not involve direct binding, processing, or enzyme activity."
lTDesc Complex_Process = "Regulatory influence that is not modeled in detail, but involves more than one molecule or a macrostructure. For example, the physical need for kinetochores on replicated sister chromatids for the assembly of certain protein complexes can be represented as a link from the node representing kinetochores to the regulatory proteins, with a Complex_Process link type."
lTDesc Persistence = "This link type represents self-loops that alter the ability of a node to stay in a particular state depending on its own current state. For example, if transcription of a protein is easier to maintain than to induce de novo, this may be encoded by a logic gate that includes the node itself and creates a self-loop. The link type of this loop is “Persistence”."
lTDesc Transcription = "Action of a transcription factor to alter the expression of the target node (mRNA or protein). Link type should be used for induction as well as repression (the link effect contains this information)."
lTDesc Translation = "Regulatory influence that controls the translation of mRNA into protein; should be used for induction as well as repression of translation."
lTDesc Ligand_Binding = "Binding of extracellular ligand to its receptor."
lTDesc Complex_Formation = "Binding even that leads to a regulatory protein complex."
lTDesc Inhibitory_Binding = "Binding even that represses the target node’s level or activity."
lTDesc Localization = "Regulatory influence that alters the localization of a molecule."
lTDesc Binding_Localization = "Binding even that alters the localization of a molecule."
lTDesc Protective_Binding = "Binding even that increases / protects the target node’s activity."
lTDesc Unbinding = "A regulatory influence that causes the target node to be released from a protein complex and change its activity (increase or decrease) as a result."
lTDesc Phosphorylation = "Phosphorylation."
lTDesc Dephosphorylation = "Dehosphorylation."
lTDesc Phosphorylation_Localization = "Phosphorylation resulting in altered protein localization."
lTDesc Ubiquitination = "Ubiquitination, usually leading to protein degradation."
lTDesc Degradation = "Regulatory influence leading to the degradation of the target molecule (more general than Ubiquitination; the latter link type should be used when appropriate)."
lTDesc GEF_Activity = "Action of a Guanine nucleotide exchange factor (GEF) leading to GTP loading onto (and usually the activation of) a GTPase."
lTDesc GAP_Activity = "Actions of a GTPase-activating protein (GAP) leading to the hydrolysis of GTP by (and usually de-activation of) a GTPase."
lTDesc Proteolysis = "Protein cleavage."
lTDesc Catalysis = "Increasing the rate of metabolite production by an enzyme."
lTDesc Epigenetic = "Process that alters gene expression via modifying chromatin condensation or altering DNA methylation."
lTDesc Transcription_Conflict = ""
lTDesc Secretion = "Secretion or shedding of a protein or other regulatory molecule to the extracellular environment."
lTDesc RNAi = "This process represents inhibitory binding of cytoplasmic mRNAs by RISC-bound microRNAs that block translation and/or enhance mRNA degradation."

-- Official descriptions of the various LinkEffects
lEDesc :: LinkEffect -> T.Text
lEDesc Undefined_LE = "Undefined Link Effect"
lEDesc Activation = "Link in which the input node aids the expression, activity, persistence or localization of the target such that the target is easier to turn/keep in an ON state. It can be used for multi-level nodes as long as these levels represent increasing intervals of activity."
lEDesc Repression = "Link in which the input node hinders the expression, activity, persistence or localization of the target such that the target is easier to turn/keep in an OFF state. It can be used for multi-level nodes as long as these levels represent increasing intervals of activity."
lEDesc Context_Dependent = "Link acting on a node in such a way that it activates it under certain conditions (e.g., when another input is OFF), but represses it when this condition is not met. The XOR gate is a good example of a context-dependent link effect. "
lEDesc Inapt = "This link-type refers to connections between complex regulatory switches (rather than molecules), where categorizing the effect of an input as Activation or Repression, or even context-depended activation or repression does not apply. This is usually the case with multi-state switches, where the 3 or more phenotypes represented by the discrete states of these switches do not have a meaningful ordering. Thus, stating that this switch is “activated” by another one is not appropriate."
