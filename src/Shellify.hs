module Shellify (Options(..), def, generateShellDotNixText, Packages, options, run) where

import Prelude hiding (takeWhile, writeFile)
import Constants

import Control.Applicative ((<|>))
import Control.Arrow ((+++))
import Control.Monad (when)
import Control.Monad.Writer (Writer)
import Data.Bool (bool)
import Data.Default.Class (Default(def))
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)
import Data.Text (intercalate, isInfixOf, isPrefixOf, pack, replace, stripPrefix, takeWhile, Text())
import Data.Text.IO (hPutStrLn, writeFile)
import qualified Data.Text.IO as Text
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import Paths_shellify (getDataFileName)
import System.Directory (doesPathExist)
import System.Exit (exitWith)
import System.IO.Error (tryIOError)
import System.IO (hGetContents, stderr)
import Text.Ginger (dict, easyRender, GVal(GVal), Pair, parseGingerFile, Run, SourcePos, (~>))

type Package = Text
type Packages = [ Package ]
data Options = Options {
    packages :: Packages
  , command :: Maybe Text
  , help :: Bool
} deriving (Show)

instance Default Options where
  def = Options [] Nothing False

instance Eq Options where
  a == b =  isEqual help
         && isEqual command
         && isEqual (sort . packages)
    where isEqual f = f a == f b

options :: Text -> [Text] -> Either Text Options
options progName = options'

  where options' :: [Text] -> Either Text Options
        options' [] = Right def
        options' (wd:wds) = case wd of
          "-h" -> handleHelpSwitch
          "--help" -> handleHelpSwitch
          "-p" -> handlePackageSwitch
          "--packages" -> handlePackageSwitch
          "--command" -> handleCommandSwitch wds
          "--run" -> handleCommandSwitch wds
          _ -> options' wds
          where handlePackageSwitch =
                  let (pkgs, remainingOptions) = consumePackageArgs wds
                  in appendPackages pkgs <$> options' remainingOptions

                handleCommandSwitch [] = Left "Argument missing to switch"
                handleCommandSwitch (hd:_) | isSwitch hd
                                           = Left "Argument missing to switch"
                handleCommandSwitch (hd:tl) = setCommand hd <$> options' tl

                handleHelpSwitch = Left $ helpText progName

                appendPackages ps opts = opts{packages=ps ++ packages opts}
                setCommand cmd opts = opts{command=Just cmd}

consumePackageArgs :: [Text] -> (Packages, [Text])
consumePackageArgs = worker []
  where worker pkgs [] = (pkgs, [])
        worker pkgs options@(hd:_) | isSwitch hd
                                   = (pkgs, options)
        worker pkgs (hd:tl) = worker (hd:pkgs) tl

run :: Options -> IO ()
run (Options{packages=[]}) = printError noPackagesError
run options = createShellFile options

isSwitch = isPrefixOf "-"

generateShellDotNixText :: Options -> IO (Either Text Text)
generateShellDotNixText (Options packages command _) =
   (+++) (pack . show)
         (easyRender context)
  <$> parseShellifyTemplate
  where pkgs = generateBuildInput <$> packages
        parameters = intercalate ", " $ uniq $ generateParameters <$> packages
        context :: GVal (Run SourcePos (Writer Text) Text)
        context = dict $ [ ("build_inputs" :: Text) ~> pkgs
                         , ("parameters" :: Text) ~> parameters
                         ]
                         <> maybe []
                                  (return . (("shell_hook" :: Text) ~>))
                                  command
        loadFileMay fn = rightToMaybe <$> tryIOError (readFile fn)
        parseShellifyTemplate =     getDataFileName "templates/shellify.nix.j2"
                                >>= parseGingerFile loadFileMay

generateBuildInput input  | "nixpkgs#" `isPrefixOf` input
                            = ("pkgs." <>) $ fromJust $ stripPrefix "nixpkgs#" input
                          | "#" `isInfixOf` input
                            = replace "#" "." input
                          | otherwise
                            = "pkgs." <> input

generateParameters :: Package -> Text
generateParameters package | "nixpkgs#" `isPrefixOf` package = pkgsImport
generateParameters package | "#" `isInfixOf` package = takeWhile (/= '#') package
generateParameters package = pkgsImport
pkgsImport = "pkgs ? import <nixpkgs> {}" :: Text

createShellFile :: Options -> IO ()
createShellFile opts =
  generateShellDotNixText opts
  >>= either printError
             writeShellFile

writeShellFile :: Text -> IO ()
writeShellFile expectedContents = do
  fileContents <-     doesPathExist "shell.nix"
                  >>= bool
                       (return Nothing)
                       (Just <$> Text.readFile "shell.nix")
  printError $ actionDescription expectedContents fileContents
  when (shouldGenerateNewFile fileContents)
    $ writeFile "shell.nix" expectedContents
  exitWith $ returnCode expectedContents fileContents

actionDescription :: Text -> Maybe Text -> Text
actionDescription _ Nothing = "shell.nix does not exist. Creating one"
actionDescription a (Just b) | a == b = "The existing shell.nix is good already"
actionDescription _ _ = "A shell.nix exists already. Delete it or move it and try again"

returnCode :: Text -> Maybe Text -> ExitCode
returnCode _ Nothing = ExitSuccess
returnCode a (Just b) | a == b = ExitSuccess
returnCode _ _ = ExitFailure 1

shouldGenerateNewFile :: Maybe Text -> Bool
shouldGenerateNewFile = (== Nothing)

uniq :: Ord a => [a] -> [a]
uniq = toList . fromList

printError = hPutStrLn stderr
rightToMaybe = either (const Nothing) Just
