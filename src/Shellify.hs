module Shellify (parseOptionsAndCalculateExpectedFiles, runShellify) where

import Prelude hiding (writeFile)
import Constants
import FlakeTemplate
import Options
import ShellifyTemplate
import TemplateGeneration

import Control.Monad (when, (>=>))
import Data.Bool (bool)
import Data.Text (pack, Text(), unpack)
import Data.Text.IO (hPutStrLn, writeFile)
import qualified Data.Text.IO as Text
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesPathExist)
import System.Exit (exitWith)
import System.IO (stderr)

createAFile (name, content) = do extCde <- createFile (unpack name) content
                                 when (extCde /= ExitSuccess)
                                   $ exitWith extCde


runShellify :: [Text] -> IO ()
runShellify(pName:args) = getRegistryDB
             >>= either
                   (printErrorAndReturnFailure . ("Error calling nix registry: " <>))
                   (\registryDB -> either printErrorAndReturnFailure
                                          (mapM_ createAFile)
                                          $ parseOptionsAndCalculateExpectedFiles registryDB pName args)


parseOptionsAndCalculateExpectedFiles :: Text -> Text -> [Text] -> Either Text [(Text,Text)]
parseOptionsAndCalculateExpectedFiles registry programName =
  fmap
    (\opts ->
        ("shell.nix", generateShellDotNixText opts)
      : maybe
            []
            (pure . ("flake.nix",))
            (generateFlakeText registry opts))
  . options programName

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

printErrorAndReturnFailure err = printError err >> exitWith (ExitFailure 1)
printError = hPutStrLn stderr

