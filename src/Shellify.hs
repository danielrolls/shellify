module Shellify (Options(..), def, generateShellDotNixText, generateFlakeText, Packages, options, run) where

import Prelude hiding (takeWhile, writeFile)
import Constants
import FlakeTemplate
import ShellifyTemplate

import Control.Applicative ((<|>))
import Control.Arrow ((+++))
import Control.Monad (when)
import Data.Bool (bool)
import Data.Default.Class (Default(def))
import Data.List (find, sort)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, toList)
import Data.Text (isInfixOf, isPrefixOf, pack, replace, splitOn, stripPrefix, takeWhile, Text(), unpack)
import Data.Text.IO (hPutStrLn, writeFile)
import qualified Data.Text.IO as Text
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesPathExist)
import System.Exit (exitWith)
import System.IO (stderr)
import Text.StringTemplate (newSTMP, render, setAttribute, StringTemplate)

type Package = Text
type Packages = [ Package ]
data Options = Options {
    packages :: Packages
  , command :: Maybe Text
  , generateFlake :: Bool
} deriving (Show)

instance Default Options where
  def = Options [] Nothing False

instance Eq Options where
  a == b =  isEqual command
         && isEqual (sort . packages)
         && isEqual generateFlake
    where isEqual f = f a == f b

data OptionsParser = OptionsParser {
  remainingOptions :: [Text]
, optionsResult :: Either Text (Options -> Options)
}

options :: Text -> [Text] -> Either Text Options
options progName args = 
  let optionsHandler | hasShellArg args = newStyleOption
                     | otherwise = oldStyleOption
      shellArgFilter | hasShellArg args = withoutShellArg
                     | otherwise = id
      optionsCaller _ (OptionsParser [] t) = t
      optionsCaller f (OptionsParser (hd:tl) res) =
        let (OptionsParser newRemaining newRes) = f hd tl
        in optionsCaller f $ OptionsParser newRemaining ((.) <$> newRes <*> res)
  in (\a -> a def) <$> optionsCaller optionsHandler (OptionsParser (shellArgFilter args) (Right id))

  where oldStyleOption :: Text -> [Text] -> OptionsParser
        oldStyleOption "-p" = handlePackageSwitch
        oldStyleOption "--packages" = handlePackageSwitch
        oldStyleOption opt = baseOption opt
        newStyleOption "-p" = returnError "-p not supported with new style commands"
        newStyleOption "--packages" = returnError "--packages not supported with new style commands"
        newStyleOption arg | isSwitch arg = baseOption arg
                           | otherwise = transformOptionsWith $ appendPackages [arg]
        baseOption :: Text -> [Text] -> OptionsParser
        baseOption "-h" = returnError $ helpText progName
        baseOption "--help" = returnError $ helpText progName
        baseOption "--verbose" = doNothing
        baseOption "--command" = handleCommandSwitch
        baseOption "--run" = handleCommandSwitch
        baseOption "--with-flake" = transformOptionsWith setFlakeGeneration
        baseOption _ = transformOptionsWith id
        doNothing = transformOptionsWith id
        transformOptionsWith fun wds = OptionsParser wds (Right fun)
        handlePackageSwitch wds = let (pkgs, remainingOptions) = consumePackageArgs wds
                                  in transformOptionsWith (appendPackages pkgs) remainingOptions
        handleCommandSwitch (hd:tl) | isSwitch hd
                                    = returnError "Argument missing to switch" tl
                                    | otherwise
                                    = transformOptionsWith (setCommand hd) tl
        handleCommandSwitch [] = returnError "Argument missing to switch" []

        appendPackages ps opts = opts{packages=ps ++ packages opts}
        setCommand cmd opts = opts{command=Just cmd}
        setFlakeGeneration opts = opts{generateFlake=True}
        returnError errorText remaining = OptionsParser remaining $ Left errorText

consumePackageArgs :: [Text] -> (Packages, [Text])
consumePackageArgs = worker []
  where worker pkgs [] = (pkgs, [])
        worker pkgs options@(hd:_) | isSwitch hd
                                   = (pkgs, options)
        worker pkgs (hd:tl) = worker (hd:pkgs) tl

run :: Options -> IO ()
run Options{packages=[]} = printErrorAndReturnFailure noPackagesError >>= exitWith
run options = do shellRes <- createShellFile options
                 maybe (exitWith shellRes)
                       (\flakeText ->
                         createFile "flake.nix" flakeText >>= exitWith . fromMaybe ExitSuccess . find (/= ExitSuccess) . (shellRes :) . pure
                       )
                       $ generateFlakeText options

generateFlakeText :: Options -> Maybe Text
generateFlakeText Options{packages=packages, generateFlake=flake} =
  bool
    Nothing
    (Just $ render $ setAttribute "repos" repos
          $ setAttribute "unknown_repos" unknownRepos
          $ setAttribute "repo_vars" repoVars
          $ newSTMP flakeTemplate)
    flake
  where repos = uniq $ getPackageRepo <$> (sort packages)
        importVars = toImportVar <$> repos
        repoVars = uniq $ getPackageRepoVarName . getPackageRepo <$> (sort packages)
        unknownRepos = filter (/= "nixpkgs") repos

generateShellDotNixText :: Options -> Text
generateShellDotNixText Options{packages=packages, command=command} =
  render
  $ setAttribute "build_inputs" pkgs
  $ setAttribute "parameters" parameters
  $ maybe id
          (setAttribute "shell_hook")
          command
  $ newSTMP shellifyTemplate
  where pkgs = generateBuildInput <$> (sort packages)
        parameters = uniq $ generateParameters <$> (sort packages)
        generateBuildInput input = (toImportVar . getPackageRepo) input <> "." <> getPackageName input

getPackageRepo input | "#" `isInfixOf` input
                        = head $ splitOn "#" input
                     | otherwise
                        = "nixpkgs"

getPackageName input | "#" `isInfixOf` input
                        = head $ tail $ splitOn "#" input
                     | otherwise
                        = input

toImportVar var | var == "nixpkgs"
                  = "pkgs"
                | otherwise
                  = var

getPackageRepoVarName "nixpkgs" = "pkgs"
getPackageRepoVarName a = a

generateParameters :: Package -> Text
generateParameters package | "nixpkgs#" `isPrefixOf` package = pkgsImport
generateParameters package | "#" `isInfixOf` package = takeWhile (/= '#') package
generateParameters package = pkgsImport
pkgsImport = "pkgs ? import <nixpkgs> {}" :: Text

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

uniq :: Ord a => [a] -> [a]
uniq = toList . fromList

printErrorAndReturnFailure err = printError err >> return (ExitFailure 1)
printError = hPutStrLn stderr
rightToMaybe = either (const Nothing) Just

isSwitch = isPrefixOf "-"

hasShellArg [] = False
hasShellArg ("shell":_) = True
hasShellArg (hd:tl) | isSwitch hd = hasShellArg tl
                    | otherwise = False

withoutShellArg [] = []
withoutShellArg ("shell":tl) = tl
withoutShellArg (hd:tl) = hd : withoutShellArg tl
