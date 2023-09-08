{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This executable generated .golden tests for importify.

module Main (main) where

import           Universum

import           Path           (Abs, Dir, File, Path, fileExtension, fromAbsFile, (-<.>))
import           Path.IO        (listDirRecur, removeFile)

import           Importify.Main (importifyFileContent)
import           Importify.Path (testDataPath)
import           System.Environment (getArgs)

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        ["--clean"] -> cleanGoldenExamples
        ["--force"] -> generateGoldenTests
        []          -> generateGoldenTestsPrompt
        _           -> putText "Incorrect arguments!"

findByExtension :: MonadIO m => String -> Path b Dir -> m [Path Abs File]
findByExtension ext path = filter ((== ext) . fe) . snd
                          <$> listDirRecur path
  where fe :: Path b File -> String
        fe filepath = case fileExtension filepath of
          Nothing -> error "this is not supposed to happen"
          Just fp -> fp

findGoldenFiles :: MonadIO m => Path b Dir -> m [Path Abs File]
findGoldenFiles = findByExtension "golden"

cleanGoldenExamples :: MonadIO m => m ()
cleanGoldenExamples = do
    goldenExamples <- findGoldenFiles testDataPath
    mapM_ removeFile goldenExamples

findHaskellFiles :: MonadIO m => Path b Dir -> m [Path Abs File]
findHaskellFiles = findByExtension ".hs"

writeBinaryFile :: MonadIO m => Path Abs File -> Text -> m ()
writeBinaryFile = writeFile . fromAbsFile

generateGoldenTestsPrompt :: IO ()
generateGoldenTestsPrompt = do
    putText "> Are you sure want to generate new golden examples? [y/N]"
    getLine >>= \case
        "y" -> generateGoldenTests
        _   -> putText "Aborting generation"

generateGoldenTests :: IO ()
generateGoldenTests = do
    testCaseFiles <- findHaskellFiles testDataPath
    forM_ testCaseFiles $ \testCasePath -> do
       Right modifiedSrc <- importifyFileContent testCasePath
       goldenPath        <- testCasePath -<.> "golden"
       writeBinaryFile goldenPath modifiedSrc
