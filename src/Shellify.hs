module Shellify (Options(..), Packages, options, run) where

import Prelude hiding (writeFile)
import Constants
import FlakeTemplate
import ShellifyTemplate

import Control.Monad (when, (>=>))
import Data.Bool (bool)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (pack, Text())
import Data.Text.IO (hPutStrLn, writeFile)
import qualified Data.Text.IO as Text
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesPathExist)
import System.Exit (exitWith)
import System.IO (stderr)

import Options
import TemplateGeneration

run :: Options -> IO ()
run Options{packages=[]} = printErrorAndReturnFailure noPackagesError >>= exitWith
run options = getRegistryDB
              >>= \case
                   Left err -> printErrorAndReturnFailure ("Error calling nix registry: " <> err) >>= exitWith
                   Right registryDB ->
                     do shellRes <- createShellFile options
                        maybe
                          (exitWith shellRes)
                          (createFile "flake.nix" >=> (exitWith . fromMaybe ExitSuccess . find (/= ExitSuccess) . (shellRes :) . pure))
                          $ generateFlakeText registryDB options

createShellFile :: Options -> IO ExitCode
createShellFile = createFile "shell.nix" . generateShellDotNixText

createFile :: FilePath -> Text -> IO ExitCode
createFile fileName expectedContents = do
  fileContents <-     doesPathExist fileName
                  >>= bool
                       (return Nothing)
                       (Just <$> Text.readFile fileName)
  printError $ actionDescription (pack fileName) expectedContents fileContents
  when (shouldGenerateNewFile fileContents)
    $ writeFile fileName expectedContents
  return $ returnCode expectedContents fileContents

actionDescription :: Text -> Text -> Maybe Text -> Text
actionDescription fName _ Nothing = fName <> " does not exist. Creating one"
actionDescription fName a (Just b) | a == b = "The existing " <> fName <> " is good already"
actionDescription fName _ _ = "A " <> fName <> " exists already. Delete it or move it and try again"

returnCode :: Text -> Maybe Text -> ExitCode
returnCode _ Nothing = ExitSuccess
returnCode a (Just b) | a == b = ExitSuccess
returnCode _ _ = ExitFailure 1

shouldGenerateNewFile :: Maybe Text -> Bool
shouldGenerateNewFile = (== Nothing)

printErrorAndReturnFailure err = printError err >> return (ExitFailure 1)
printError = hPutStrLn stderr

