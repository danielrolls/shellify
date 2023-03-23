{-# LANGUAGE LambdaCase #-}
module Lib.Shellify (Options(Options), Packages, options, runtm) where

import Prelude hiding (concat, readFile, writeFile)
import Text.ParserCombinators.Parsec (GenParser, many, many1, oneOf, char, parse, anyChar)
import Data.Text (pack, unpack, Text(), isPrefixOf, concat)
import Data.List (elemIndex, findIndex)
import Control.Applicative ((<|>))
import Text.Ginger hiding (length)
import qualified Data.HashMap.Strict as HashMap
import System.IO (openFile, hGetContents, hPutStrLn, hPutStr, stderr)
import System.IO.Error (tryIOError)
import System.Exit (exitFailure)
import Data.Maybe (fromMaybe)
import Control.Monad.Identity (join)
import Paths_shellify (getDataFileName)
import Data.Text.IO (readFile, writeFile)
import System.Directory (doesPathExist)

type Packages = [ Text ]
newtype Options = Options Packages deriving (Eq, Show)

options :: [Text] -> Options
options = Options . options'

options' wds = 
  let packagesSwitchPos =    elemIndex "-p" wds
                         <|> elemIndex "--packages" wds
      firstSwitchPosInfollowingArg = join . fmap (findIndex ("-" `isPrefixOf`)
                                                 . (`drop` wds) 
                                                 . (+1)
                                                 )
                                        $ packagesSwitchPos
      trailingArgsToTake = fromMaybe (length wds)
                                     firstSwitchPosInfollowingArg
  in maybe []
           (\i -> drop (i+1) (take (i+1+trailingArgsToTake) wds)
                  <> options' (drop (i+1+trailingArgsToTake) wds)
           )
           packagesSwitchPos

runtm :: Options -> IO ()
runtm (Options packages) = 
  do  getDataFileName "templates/shellify.nix.j2" 
  >>= parseGingerFile loadFileMay 
  >>= either handleParseError
             (createShellFile . easyRender context)
  where pkgsStr = ("[" <>) . (<> " ]") . concat . fmap (" pkgs." <>) $ packages
        context = HashMap.fromList [ ("build_inputs" :: Text, pkgsStr) ]
	handleParseError err = print err >> exitFailure
        loadFileMay fn =
          tryIOError (loadFile fn) >>= 
           (\case
              Right contents -> return (Just contents)
              Left _ -> return Nothing)
          where
	    loadFile fn' = unpack <$> readFile fn'

createShellFile :: Text -> IO ()
createShellFile expectedContents = do
  exist <- doesPathExist "shell.nix"
  if not exist then do
     hPutStrLn stderr "shell.nix does not exist. Creating one"
     writeFile "shell.nix" expectedContents
  else do
    fileContents <- readFile "shell.nix"
    if fileContents == expectedContents then
      hPutStrLn stderr "The existing shell.nix is good already"
    else 
      hPutStrLn stderr "A shell.nix exists already. Delete it or move it and try again"
