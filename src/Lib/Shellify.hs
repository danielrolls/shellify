module Lib.Shellify (Options(Options), Packages, options, run) where

import Prelude hiding (concat, writeFile)

import Control.Applicative ((<|>))
import Control.Monad.Identity (join)
import Data.HashMap.Strict (fromList, insert)
import Data.Maybe (fromMaybe)
import Data.Text.IO (writeFile)
import qualified Data.Text.IO as Text
import Data.Text (pack, unpack, Text(), isPrefixOf, concat)
import Paths_shellify (getDataFileName)
import System.Directory (doesPathExist)
import System.IO.Error (tryIOError)
import System.IO (openFile, hGetContents, hPutStrLn, stderr)
import Text.Ginger (easyRender, parseGingerFile)

type Packages = [ Text ]
data Options = Options {
    packages :: Packages
  , command :: Maybe Text
} deriving (Eq, Show)

options :: [Text] -> Either Text Options
options = options' (Right $ Options [] Nothing)

options' :: Either Text Options -> [Text] -> Either Text Options
options' retOptions [] = retOptions
options' retOptions (wd:wds) = case wd of
  "-p" -> handlePackageSwitch
  "--packages" -> handlePackageSwitch
  "--command" -> handleCommandSwitch wds
  "--run" -> handleCommandSwitch wds
  _ -> options' retOptions wds
  where handlePackageSwitch = let (pkgs, remainingOptions) = consumePackageArgs wds
                              in options' (fmap (\r -> r{packages=pkgs ++ packages r}) retOptions) remainingOptions
        handleCommandSwitch [] = Left "Argument missing to switch"
        handleCommandSwitch (hd:_) | isSwitch hd
                                    = Left "Argument missing to switch"
        handleCommandSwitch (hd:tl) = options' (fmap (\r -> r{command=Just hd}) retOptions) tl

consumePackageArgs :: [Text] -> (Packages, [Text])
consumePackageArgs = worker []
  where worker pkgs [] = (pkgs, [])
	worker pkgs options@(hd:_) | isSwitch hd = (pkgs, options)
	worker pkgs (hd:tl) = worker (hd:pkgs) tl

run :: Options -> IO ()
run (Options [] _) = printError "I can't write out a shell file without any packages specified"
run options = createShellFile options

isSwitch = ("-" `isPrefixOf`)

createShellFile :: Options -> IO ()
createShellFile (Options packages command) =
  do  getDataFileName "templates/shellify.nix.j2" 
  >>= parseGingerFile loadFileMay 
  >>= either handleParseError
             (writeShellFile . easyRender context)
  where pkgsStr = ("[" <>) . (<> " ]") . concat . fmap (" pkgs." <>) $ packages
        context = maybe id
                        (insert "shell_hook")
                        command
                  $ fromList [ ("build_inputs" :: Text, pkgsStr) ]
        handleParseError err = printError (show err)
	loadFileMay fn = rightToMaybe <$> tryIOError (readFile fn)

writeShellFile :: Text -> IO ()
writeShellFile expectedContents = do
  exist <- doesPathExist "shell.nix"
  if exist then do
    fileContents <- Text.readFile "shell.nix"
    if fileContents == expectedContents then
      printError "The existing shell.nix is good already"
    else 
      printError "A shell.nix exists already. Delete it or move it and try again"
  else do
     printError "shell.nix does not exist. Creating one"
     writeFile "shell.nix" expectedContents

printError = hPutStrLn stderr
rightToMaybe = either (const Nothing) Just
