module Lib.Shellify (Options(..), def, Packages, options, run) where

import Prelude hiding (concat, writeFile)

import Control.Applicative ((<|>))
import Control.Monad.Identity (join)
import Data.Default.Class (Default(def))
import Data.HashMap.Strict (fromList, insert)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text.IO (writeFile)
import qualified Data.Text.IO as Text
import Data.Text (pack, unpack, Text(), isPrefixOf, concat)
import Paths_shellify (getDataFileName)
import System.Directory (doesPathExist)
import System.IO.Error (tryIOError)
import System.IO (openFile, hGetContents, hPutStrLn, stderr)
import Text.Ginger (easyRender, parseGingerFile)
import Text.RawString.QQ (r)

type Packages = [ Text ]
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

helpText progName = "USAGE: " <> progName <> (pack [r| -p [PACKAGES] 

Pass nix-shell arguments to nix-shellify to have it generate a shell.nix in
the current directory. You can then just run nix-shell in that directory to
have those directories in your environment. To run nix-shell you must first
install Nix.|])

consumePackageArgs :: [Text] -> (Packages, [Text])
consumePackageArgs = worker []
  where worker pkgs [] = (pkgs, [])
        worker pkgs options@(hd:_) | isSwitch hd
                                   = (pkgs, options)
        worker pkgs (hd:tl) = worker (hd:pkgs) tl

run :: Options -> IO ()
run (Options{packages=[]}) =
  printError [r|I can't write out a shell file without any packages specified.
Try 'nix-shellify --help' for more information.|]
run options = createShellFile options

isSwitch = isPrefixOf "-"

createShellFile :: Options -> IO ()
createShellFile (Options packages command _) =
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
