module Main where

import Data.Text (pack)
import Shellify (runShellify)
import System.Environment (getArgs, getProgName)

main :: IO ()
main = do
  progName <- pack <$> getProgName
  getTextArgs
    >>= runShellify . (<>) [progName]

getTextArgs = fmap pack <$> getArgs
