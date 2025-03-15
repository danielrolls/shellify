module Main where

import Shellify (runShellify, printErrorAndReturnFailure)
import System.Environment (getArgs)
import Options(parseCommandLine)
import Options.Applicative (handleParseResult)
import Control.Monad ((>=>))

main :: IO ()
main = getArgs >>=
         either printErrorAndReturnFailure
                (handleParseResult >=> runShellify )
         . parseCommandLine