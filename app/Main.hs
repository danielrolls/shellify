module Main where

import Data.Text.IO (hPutStrLn)
import Data.Text (pack)
import Lib.Shellify (options, run)
import System.Environment (getArgs, getProgName)
import System.IO (stderr)

main :: IO ()
main = do
  progName <- pack <$> getProgName
  args <- getArgs
  either (hPutStrLn stderr)
         run
    <$> options progName $ fmap pack args
