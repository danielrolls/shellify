module TestHelpers where

import Prelude hiding (last, putStrLn, readFile, reverse, tail, words)
import Data.Bool (bool)
import Data.Text (isInfixOf, last, reverse, tail, Text(), unpack, words)
import Data.Text.IO (putStrLn, readFile)
import Test.Hspec (Expectation(), expectationFailure, it, shouldBe, shouldContain)

import Options
import Shellify
import TemplateGeneration
import TestConstants

shouldReturnSubstring shellifyOutput expectedSubstring =
    either
      ((`shouldContain` expectedSubstring) . unpack)
      (const (expectationFailure "expected Left but got Right"))
      shellifyOutput

shellifyWithArgs :: Text -> Either Text [(Text, Text)]
shellifyWithArgs = parseOptionsAndCalculateExpectedFiles db "nix-shellify" . words

shouldReturnShellAndFlakeTextDefinedBy result expectedOutput =
     it "should produce the expected shell.nix and flake.nix" $
       do expShell <- readNixTemplate (shellFile expectedOutput)
          expFlake <- readNixTemplate (flakeFile expectedOutput)
          result `shouldBe`
              Right [("shell.nix", expShell),("flake.nix", expFlake)]

shouldReturnShellTextDefinedBy result expectedOutput =
     it "should produce the expected shell.nix" $
       do expShell <- readNixTemplate (shellFile expectedOutput)
          either
	    (const $ expectationFailure "Expected Right but got Left")
	    (\((fileName, shellNixOutput):_) ->
                  do shellNixOutput `shouldBe` expShell
                     fileName `shouldBe` "shell.nix")
	    result

theOptions = options "nix-shellify" . words

shouldResultInPackages :: Text -> [Text] -> Expectation
shouldResultInPackages parameters packages =
     theOptions parameters
       `shouldBe`
     Right def{packages=packages}

instance Show Options

readNixTemplate :: FilePath -> IO Text
readNixTemplate fileName =
    stripTrailingNewline <$> readFile ("test/outputs/" <> fileName)
    where stripTrailingNewline f = bool id stripLastChar (lastCharIsNewline f) f
          lastCharIsNewline = (== '\n') . last
          stripLastChar = reverse . tail . reverse

flakeFile = (<> "-flake.nix")
shellFile = (<> "-shell.nix")
