module TemplateGeneration (generateShellDotNixText, generateFlakeText, getRegistryDB) where

import Constants
import FlakeTemplate
import Options
import ShellifyTemplate

import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.List (find, sort, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Set (fromList, toList)
import Data.Text (Text(), isInfixOf, isPrefixOf, pack, splitOn, unpack)
import Development.Shake.Command (cmd, Exit(Exit), Stderr(Stderr), Stdout(Stdout))
import System.Exit (ExitCode (ExitSuccess))
import Text.ParserCombinators.Parsec (Parser, char, endBy, eof, many1, noneOf, parse, string, (<|>))
import Text.StringTemplate (newSTMP, render, setAttribute)

generateFlakeText :: Text -> Options -> Maybe Text
generateFlakeText db Options{packages=Packages packages, outputForm=outputForm, prioritiseLocalPinnedSystem=prioritiseLocalPinnedSystem} =
  bool
    Nothing
    (Just $ render
          $ setAttribute "repo_inputs" repoInputs
          $ setAttribute "repos" repos
          $ setAttribute "pkgs_decls" pkgsDecls
          $ setAttribute "shell_args" shellArgs
          $ newSTMP flakeTemplate)
    (outputForm == Flake)
  where repos = getPackageRepoWrapper packages
        repoVars = getPackageRepoVarName <$> repos
        repoInputs = repoInput <$> repos
        repoInputLine repoName url = repoName <> ".url = \"" <> url <> "\";"
        repoInput repoName = repoInputLine repoName .
          either
            (error . ("Unexpected output from nix registry call: " <>))
            (fromMaybe "PLEASE ENTER input here")
            . findFlakeRepoUrl prioritiseLocalPinnedSystem db $ repoName
        pkgsVar = (<> "Pkgs")
        pkgsVars = pkgsVar <$> repos
        pkgsDecls = (\repo -> pkgsDecl (pkgsVar repo) repo) <$> repos
        shellArgs = (\(a,b) -> a <> "=" <> b <> ";") <$> zip repoVars pkgsVars 

generateShellDotNixText :: Options -> Text
generateShellDotNixText Options{packages=Packages packages, command=command} =
  render
  $ setAttribute "build_inputs" pkgs
  $ setAttribute "parameters" parameters
  $ maybe id
          (setAttribute "shell_hook")
          command
  $ newSTMP shellifyTemplate
  where pkgs = generateBuildInput <$> sort packages
        parameters = generateParametersWrapper packages
        generateBuildInput input = (toImportVar . getPackageRepo) input <> "." <> getPackageName input

getPackageRepoWrapper :: [Package] -> [Text]
getPackageRepoWrapper = uniq . ("nixpkgs" :) . fmap getPackageRepo . sort

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

generateParametersWrapper :: [Package] -> [Text]
generateParametersWrapper = uniq . ("pkgs ? import <nixpkgs> {}" :) . fmap generateParameters . sort

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

findFlakeRepoUrl :: Bool -> Text -> Text -> Either String (Maybe Text)
findFlakeRepoUrl prioritiseLocalPinnedSystem haystack needle =
  bimap ((<>) "Error processing nix registry list output: " . show)
        (fmap repoUrl . find ((needle ==) . repoName)
                       . (if prioritiseLocalPinnedSystem then sortOn repoType else sortBy compareRepoEntries))
        $ parse parseRepos "" . unpack $ haystack

compareRepoEntries repoA repoB
  | repoHasLocalPinning repoA && not (repoHasLocalPinning repoB) = GT
  | repoHasLocalPinning repoB && not (repoHasLocalPinning repoA) = LT
  | otherwise = repoType repoA `compare` repoType repoB
  where repoHasLocalPinning = isPrefixOf "path:" . repoUrl

data RepoType = User | System | Global
                deriving (Eq, Ord)

data FlakeRepo = FlakeRepo {
    repoName :: Text
  , repoUrl :: Text
  , repoType :: RepoType
}

parseRepos :: Parser [FlakeRepo]
parseRepos = do res <- endBy parseLine (char '\n')
                eof
                return res
  where parseLine = do repoType <- parseRepoType
                       char ' '
                       flakeName <- string "flake:" >> parseParam
                       char ' '
                       repoUrl <- parseParam
                       return $ FlakeRepo (pack flakeName) (pack repoUrl) repoType
        parseParam = many1 (noneOf " \n")
        parseRepoType = (string "global" >> return Global)
                    <|> (string "system" >> return System)
                    <|> (string "user" >> return User)

