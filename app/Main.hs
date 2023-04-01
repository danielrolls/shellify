module Main where

import Data.Text.IO (hPutStrLn)
import Data.Text (pack)
import Lib.Shellify (options, run)
import System.Environment (getArgs)
import System.IO (stderr)

main :: IO ()
main = 
  options . fmap pack <$> getArgs
  >>= either (hPutStrLn stderr)
             run 
