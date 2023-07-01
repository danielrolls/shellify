module TemplateGeneration (generateShellDotNixText, generateFlakeText, getRegistryDB) where

import Prelude hiding (lines)

import Constants
import FlakeTemplate
import Options
import ShellifyTemplate

import Data.Bool (bool)
import Data.List (find, sort)
import Data.List.Extra ((!?))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (fromList, toList)
import Data.Text (isInfixOf, isPrefixOf, lines, pack, splitOn, Text())
import Development.Shake.Command (cmd, Exit(Exit), Stderr(Stderr), Stdout(Stdout))
import System.Exit (ExitCode (ExitSuccess))
import Text.StringTemplate (newSTMP, render, setAttribute)

generateFlakeText :: Text -> Options -> Maybe Text
generateFlakeText db Options{packages=packages, generateFlake=shouldGenerateFlake} =
  bool
    Nothing
    (Just $ render
          $ setAttribute "repo_inputs" repoInputs
          $ setAttribute "repos" repos
          $ setAttribute "pkgs_decls" pkgsDecls
          $ setAttribute "shell_args" shellArgs
          $ newSTMP flakeTemplate)
    shouldGenerateFlake
  where repos = uniq $ getPackageRepo <$> sort packages
        repoVars = uniq $ getPackageRepoVarName . getPackageRepo <$> sort packages
        repoInputs = repoInput <$> repos
        repoInputLine repoName url = repoName <> ".url = \"" <> url <> "\";"
        repoInput repoName = repoInputLine repoName .
          either
            (error "Unexpected output from nix registry call: " <>)
            (fromMaybe "PLEASE ENTER input here")
            . findFlakeRepoUrl db $ repoName
        pkgsVar = (<> "Pkgs")
        pkgsVars = pkgsVar <$> repos
        pkgsDecls = (\repo -> pkgsDecl (pkgsVar repo) repo) <$> repos
        shellArgs = (\(a,b) -> a <> "=" <> b <> ";") <$> zip repoVars pkgsVars 

generateShellDotNixText :: Options -> Text
generateShellDotNixText Options{packages=packages, command=command} =
  render
  $ setAttribute "build_inputs" pkgs
  $ setAttribute "parameters" parameters
  $ maybe id
          (setAttribute "shell_hook")
          command
  $ newSTMP shellifyTemplate
  where pkgs = generateBuildInput <$> sort packages
        parameters = uniq $ generateParameters <$> sort packages
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
generateParameters package | "#" `isInfixOf` package
                           && not ("nixpkgs#" `isPrefixOf` package)
                           = getPackageRepo package
generateParameters _ = "pkgs ? import <nixpkgs> {}"

uniq :: Ord a => [a] -> [a]
uniq = toList . fromList

getRegistryDB :: IO (Either Text Text)
getRegistryDB =
     do (Stdout out, Stderr err, Exit ex) <- cmd
          ("nix --extra-experimental-features nix-command --extra-experimental-features flakes registry list" :: String)
        return $ bool (Left $ pack err)
                      (Right $ pack out)
                      (ex == ExitSuccess)

findFlakeRepoUrl :: Text -> Text -> Either Text (Maybe Text)
findFlakeRepoUrl haystack needle =
       fmap repoUrl . find ((needle ==) . repoName) . catMaybes <$> mapM getFlakeRepo (lines haystack)

data FlakeRepo = FlakeRepo {
    repoName :: Text
  , repoUrl :: Text
}

getFlakeRepo :: Text -> Either Text (Maybe FlakeRepo)
getFlakeRepo line = let expectedField = maybe (Left "unexepected nix registry command format")
                                              Right
                                        . (!?) (splitOn " " line)
                        urlField = expectedField 2
                        splitRepoField = splitOn ":" <$> expectedField 1
                        potentialFlakeName ["flake", b] = Just b
                        potentialFlakeName _ = Nothing
                        f x y = (`FlakeRepo` y) <$> potentialFlakeName x
                     in f <$> splitRepoField <*> urlField

