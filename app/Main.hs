{-| Tool for managing import sections.

    Remove redundant imports algorithm (current version):
      1. For every import declaration that in @loadEnvironment@
         traverse list of import names and collect those that are not in module.
      2. Remove every name from corresponding imports lists.
      3. Print new modified version of file with imports changed.
 -}

module Main (main) where

import Universum

import System.Wlog (severityPlus)

import Extended.System.Wlog (initImportifyLogger)
import Importify.Environment (runCache)
import Importify.Main (importifyCacheList, importifyCacheProject, importifyFileOptions)

import Options (CabalCacheOptions (..), Command (..), ImportifyCliArgs (..), SingleFileOptions (..),
                coLoggingSeverity, parseOptions)

main :: IO ()
main = do
    ImportifyCliArgs{..} <- parseOptions
    initImportifyLogger (severityPlus $ coLoggingSeverity icaCommon)
    case icaCommand of
        SingleFile sfOpts -> importifySingleFile sfOpts
        CabalCache ccOpts -> buildCabalCache ccOpts

importifySingleFile :: SingleFileOptions -> IO ()
importifySingleFile SingleFileOptions{..} =
    importifyFileOptions sfoOutput sfoFileName

buildCabalCache :: CabalCacheOptions -> IO ()
buildCabalCache CabalCacheOptions{..} =
    runCache ccoSaveSources $ case ccoDependencies of
        []     -> importifyCacheProject
        (d:ds) -> importifyCacheList (d :| ds)
