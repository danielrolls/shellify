module TestHelpers where

import Prelude hiding (last, putStrLn, readFile, reverse, tail, unlines, words)
import Data.Bool (bool)
import Data.Text (isInfixOf, last, reverse, tail, Text(), unpack, unlines, words)
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
shellifyWithArgs = shellifyWithArgsWithDb realDbExample

realDbExample =
  [ "global flake:agda github:agda/agda"
  , "global flake:arion github:hercules-ci/arion"
  , "global flake:blender-bin github:edolstra/nix-warez?dir=blender"
  , "global flake:composable github:ComposableFi/composable"
  , "global flake:dreampkgs github:nix-community/dreampkgs"
  , "global flake:dwarffs github:edolstra/dwarffs"
  , "global flake:emacs-overlay github:nix-community/emacs-overlay"
  , "global flake:fenix github:nix-community/fenix"
  , "global flake:flake-parts github:hercules-ci/flake-parts"
  , "global flake:flake-utils github:numtide/flake-utils"
  , "global flake:gemini github:nix-community/flake-gemini"
  , "global flake:hercules-ci-effects github:hercules-ci/hercules-ci-effects"
  , "global flake:hercules-ci-agent github:hercules-ci/hercules-ci-agent"
  , "global flake:home-manager github:nix-community/home-manager"
  , "global flake:hydra github:NixOS/hydra"
  , "global flake:mach-nix github:DavHau/mach-nix"
  , "global flake:nimble github:nix-community/flake-nimble"
  , "global flake:nix github:NixOS/nix"
  , "global flake:nix-darwin github:LnL7/nix-darwin"
  , "global flake:nixops github:NixOS/nixops"
  , "global flake:nixos-hardware github:NixOS/nixos-hardware"
  , "global flake:nixos-homepage github:NixOS/nixos-homepage"
  , "global flake:nixos-search github:NixOS/nixos-search"
  , "global flake:nur github:nix-community/NUR"
  , "global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-unstable"
  , "global flake:templates github:NixOS/templates"
  , "global flake:patchelf github:NixOS/patchelf"
  , "global flake:poetry2nix github:nix-community/poetry2nix"
  , "global flake:nix-serve github:edolstra/nix-serve"
  , "global flake:nickel github:tweag/nickel"
  , "global flake:bundlers github:NixOS/bundlers"
  , "global flake:pridefetch github:SpyHoodle/pridefetch"
  , "global flake:systems github:nix-systems/default"
  , "global flake:helix github:helix-editor/helix"
  , "global flake:sops-nix github:Mic92/sops-nix"
  ]

shellifyWithArgsWithDb :: [Text] -> Text -> Either Text [(Text, Text)]
shellifyWithArgsWithDb customDb = parseOptionsAndCalculateExpectedFiles (unlines customDb) "nix-shellify" . words

whereAUserNixpkgsExistsShellifyWithArgs :: Text -> Either Text [(Text, Text)]
whereAUserNixpkgsExistsShellifyWithArgs = shellifyWithArgsWithDb
  [ "global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-global-registry"
  , "system flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-system-registry"
  , "user flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-user-registry"
  ]

whereASystemAndGlobalCustomPkgsExistsShellifyWithArgs :: Text -> Either Text [(Text, Text)]
whereASystemAndGlobalCustomPkgsExistsShellifyWithArgs = shellifyWithArgsWithDb
  [ "global flake:custompkgs github:NixOS/custompkgs/custompkgs-global-registry"
  , "system flake:custompkgs github:NixOS/custompkgs/custompkgs-system-registry"
  , "global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-unstable"
  ]

whereAGlobalAndSystemCustomPkgsExistsShellifyWithArgs :: Text -> Either Text [(Text, Text)]
whereAGlobalAndSystemCustomPkgsExistsShellifyWithArgs = shellifyWithArgsWithDb
  [ "system flake:custompkgs github:NixOS/custompkgs/custompkgs-system-registry"
  , "global flake:custompkgs github:NixOS/custompkgs/custompkgs-global-registry"
  , "global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-unstable"
  ]

whereASystemAndGlobalNixpkgsExistsShellifyWithArgs :: Text -> Either Text [(Text, Text)]
whereASystemAndGlobalNixpkgsExistsShellifyWithArgs = shellifyWithArgsWithDb
  [ "system flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-system-registry"
  , "global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-global-registry"
  ]

whereAGlobalAndSystemNixpkgsExistsShellifyWithArgs :: Text -> Either Text [(Text, Text)]
whereAGlobalAndSystemNixpkgsExistsShellifyWithArgs = shellifyWithArgsWithDb
  [ "global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-global-registry"
  , "system flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-system-registry"
  ]

whereOnlyAGlobalNixpkgsExistsShellifyWithArgs :: Text -> Either Text [(Text, Text)]
whereOnlyAGlobalNixpkgsExistsShellifyWithArgs = shellifyWithArgsWithDb
  [ "global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-global-registry"
  ]

whereALocalSystemAndFlakeGlobalNixpkgsExistsShellifyWithArgs :: Text -> Either Text [(Text, Text)]
whereALocalSystemAndFlakeGlobalNixpkgsExistsShellifyWithArgs = shellifyWithArgsWithDb
  [ "system flake:nixpkgs path:/local/nixpkgs/path"
  , "global flake:nixpkgs github:NixOS/nixpkgs/nixpkgs-global-registry"
  ]

whereALocalSystemNixpkgsExistsShellifyWithArgs :: Text -> Either Text [(Text, Text)]
whereALocalSystemNixpkgsExistsShellifyWithArgs = shellifyWithArgsWithDb
  [ "system flake:nixpkgs path:/local/nixpkgs/path"
  , "global flake:templates github:NixOS/templates"
  ]

shouldReturnShellAndFlakeTextDefinedBy :: Either Text [(Text, Text)] -> FilePath -> Expectation
shouldReturnShellAndFlakeTextDefinedBy result expectedOutput =
   do expShell <- readNixTemplate (shellFile expectedOutput)
      expFlake <- readNixTemplate (flakeFile expectedOutput)
      result `shouldBe`
          Right [("shell.nix", expShell),("flake.nix", expFlake)]

shouldReturnShellTextDefinedBy result expectedOutput =
       do expShell <- readNixTemplate (shellFile expectedOutput)
          either
            (const $ expectationFailure "Expected Right but got Left")
            (\((fileName, shellNixOutput):_) ->
                  do shellNixOutput `shouldBe` expShell
                     fileName `shouldBe` "shell.nix")
            result

shouldResultInPackages :: Text -> [Text] -> Expectation
shouldResultInPackages parameters packages =
     theOptions parameters
       `shouldBe`
     Right def{packages=packages}

theOptions = options "nix-shellify" . words

instance Show Options

readNixTemplate :: FilePath -> IO Text
readNixTemplate fileName =
    stripTrailingNewline <$> readFile ("test/outputs/" <> fileName)
    where stripTrailingNewline f = bool id stripLastChar (lastCharIsNewline f) f
          lastCharIsNewline = (== '\n') . last
          stripLastChar = reverse . tail . reverse

flakeFile = (<> "-flake.nix")
shellFile = (<> "-shell.nix")

