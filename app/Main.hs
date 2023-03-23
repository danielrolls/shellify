module Main where

import Lib.Shellify (options, runtm)
import System.Environment (getArgs)
import Data.Text (pack)

main :: IO ()
main = getArgs >>= runtm . options . map pack
