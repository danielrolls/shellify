module Shellify (printErrorAndReturnFailure, runShellify, calculateExpectedFiles) where

import Prelude hiding (readFile, writeFile)
import Options (Options())
import TemplateGeneration ( generateFlakeText, generateShellDotNixText, getRegistryDB)

import Control.Monad (guard, when)
import Data.Bool (bool)
import Data.Maybe (isNothing)
import Data.Text (pack, Text(), unpack)
import Data.Text.IO (hPutStrLn, readFile, writeFile)
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesPathExist)
import System.Exit (exitWith)
import System.IO (stderr)

runShellify :: Options -> IO ()
runShellify opts =
     getRegistryDB >>=
        either
          (printErrorAndReturnFailure . ("Error calling nix registry: " <>))
          (mapM_ createAFile . (`calculateExpectedFiles` opts))

createAFile :: (Text, Text) -> IO ()
createAFile (name, content) = do extCde <- createFile (unpack name) content
                                 when (extCde /= ExitSuccess)
                                   $ exitWith extCde

  where createFile :: FilePath -> Text -> IO ExitCode
        createFile fileName expectedContents = do
          fileContents <- traverse readFile . bool Nothing
                                                   (Just fileName)
                                                   =<< doesPathExist fileName
          printError $ actionDescription (pack fileName) expectedContents fileContents
          when (isNothing fileContents)
            $ writeFile fileName expectedContents
          return $ returnCode expectedContents fileContents

calculateExpectedFiles :: Text -> Options -> [(Text,Text)]
calculateExpectedFiles registry options =
     ("shell.nix", generateShellDotNixText options)
      : maybe
            []
            (pure . ("flake.nix",))
            (generateFlakeText registry options)

actionDescription :: Text -> Text -> Maybe Text -> Text
actionDescription fName _ Nothing = fName <> " does not exist. Creating one"
actionDescription fName a (Just b) | a == b = "The existing " <> fName <> " is good already"
actionDescription fName _ _ = "A " <> fName <> " exists already. Delete it or move it and try again"

returnCode :: Text -> Maybe Text -> ExitCode
returnCode _ Nothing = ExitSuccess
returnCode a (Just b) | a == b = ExitSuccess
returnCode _ _ = ExitFailure 1

printErrorAndReturnFailure err = printError err >> exitWith (ExitFailure 1)
printError = hPutStrLn stderr
