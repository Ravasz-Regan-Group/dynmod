{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Parse.DMMS
import qualified ReadWrite as RW
import DynmodIO
import Input
import Path
import Path.IO
import qualified Options.Applicative as O
import qualified Data.Text as T
import qualified Text.Pretty.Simple as PS
import qualified Text.Megaparsec as M
import GHC.Conc (par, pseq)

main :: IO ()
main = do
    clInput <- O.execParser opts
    mFilePath <- resolveFile' $ T.unpack $ fileN clInput
    (_, dmmsFileExt) <- splitExtension mFilePath
    fileclInput <- RW.readFile mFilePath
    let strFileName = toFilePath mFilePath
    case dmmsFileExt of
        ".dmms" ->
            case activity clInput of 
                Update upd -> updateDMMS mFilePath fileclInput upd
                Compare compareText -> do
                    cFilePath <- resolveFile' $ T.unpack $ compareText
                    (_, cExt) <- splitExtension cFilePath
                    compFileContent <- RW.readFile cFilePath
                    let compFileName = toFilePath cFilePath
                        lComp = M.runParser modelFileParse
                                            strFileName
                                            fileclInput
                        rComp = M.runParser modelFileParse
                                            compFileName
                                            compFileContent
                    case cExt of 
                        ".dmms" ->
                            case lComp `par` (rComp `pseq` (lComp, rComp)) of
                                (Left err1, Left err2) -> do
                                    PS.pPrint (M.errorBundlePretty err1)
                                    PS.pPrint (M.errorBundlePretty err2)
                                (Left err, _) ->
                                    PS.pPrint (M.errorBundlePretty err)
                                (_, Left err) ->
                                    PS.pPrint (M.errorBundlePretty err)
                                (Right leftP, Right rightP) -> 
                                    dmmsCompare mFilePath
                                                cFilePath ((fst . snd) leftP)
                                                          ((fst . snd) rightP)
                        _ ->
                            fail $ "Not a dmms file: " <> toFilePath cFilePath
                Procedure ps -> procedureDMMS mFilePath ps pOut
                Experiment es -> experimentDMMS mFilePath es pOut
                Figure fs -> figureDMMS mFilePath fs pOut
                where pOut = M.runParser modelFileParse strFileName fileclInput
        _ -> fail $ "Not a dmms file: " <> strFileName


