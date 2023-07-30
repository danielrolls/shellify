module TestHelpers where

import Prelude hiding (last, putStrLn, readFile, reverse, tail, words)
import Data.Bool (bool)
import Data.Text (isInfixOf, last, reverse, tail, Text(), unpack, words)
import Data.Text.IO (putStrLn, readFile)
import Test.Hspec (Expectation(), expectationFailure, it, shouldBe, shouldContain)

import Options
import Shellify
import TemplateGeneration

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

db = "global flake:agda github:agda/agda\nglobal flake:arion github:hercules-ci/arion\nglobal flake:blender-bin github:edolstra/nix-warez?dir=blender\nglobal flake:composable github:ComposableFi/composable\nglobal flake:dreampkgs github:nix-community/dreampkgs\nglobal flake:dwarffs github:edolstra/dwarffs\nglobal flake:emacs-overlay github:nix-community/emacs-overlay\nglobal flake:fenix github:nix-community/fenix\nglobal flake:flake-parts github:hercules-ci/flake-parts\nglobal flake:flake-utils github:numtide/flake-utils\nglobal flake:gemini github:nix-community/flake-gemini\nglobal flake:hercules-ci-effects github:hercules-ci/hercules-ci-effects\nglobal flake:hercules-ci-agent github:hercules-ci/hercules-ci-agent\nglobal flake:home-manager github:nix-community/home-manager\nglobal flake:hydra github:NixOS/hydra\nglobal flake:mach-nix github:DavHau/mach-nix\nglobal flake:nimble github:nix-community/flake-nimble\nglobal flake:nix github:NixOS/nix\nglobal flake:nix-darwin github:LnL7/nix-darwin\nglobal flake:nixops github:NixOS/nixops\nglobal flake:nixos-hardware github:NixOS/nixos-hardware\nglobal flake:nixos-homepage github:NixOS/nixos-homepage\nglobal flake:nixos-search github:NixOS/nixos-search\nglobal flake:nur github:nix-community/NUR\nglobal flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-unstable\nglobal flake:templates github:NixOS/templates\nglobal flake:patchelf github:NixOS/patchelf\nglobal flake:poetry2nix github:nix-community/poetry2nix\nglobal flake:nix-serve github:edolstra/nix-serve\nglobal flake:nickel github:tweag/nickel\nglobal flake:bundlers github:NixOS/bundlers\nglobal flake:pridefetch github:SpyHoodle/pridefetch\nglobal flake:systems github:nix-systems/default\nglobal flake:helix github:helix-editor/helix\nglobal flake:sops-nix github:Mic92/sops-nix\n" :: Text
