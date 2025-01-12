import Test.Hspec (describe, hspec, it, shouldBe)

import Options
import TestHelpers


main = hspec $ do

  describe "where system is local and global is available fall through to global" $
    whereALocalSystemAndFlakeGlobalNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "cowsay-from-global-nixpkgs"

  describe "where system is local and no user or global is available, use local" $
    whereALocalSystemNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "cowsay-from-local-nixpkgs"

  describe "When the nixpkgs in the registry is configured such that it defines a user registry" $
    whereOnlyAUserNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "cowsay-from-user-nixpkgs"

  describe "When the custompkgs in the registry is configured such that it defines a global and system registry" $
    whereASystemAndGlobalCustomPkgsExistsShellifyWithArgs "shell custompkgs#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "cowsay-from-system-custompkgs"

  describe "When the custompkgs in the registry is configured such that it defines a system and global registry" $
    whereAGlobalAndSystemCustomPkgsExistsShellifyWithArgs "shell custompkgs#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "cowsay-from-system-custompkgs"

  describe "When the nixpkgs in the registry is configured such that it defines a system registry" $
    whereASystemAndGlobalNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "cowsay-from-system-nixpkgs"

  describe "When the nixpkgs in the registry is configured such that it defines a system registry with the global registry listed first" $
    whereAGlobalAndSystemNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "cowsay-from-system-nixpkgs"

  describe "When the nixpkgs in the registry is configured such that it defines only a global registry" $
    whereOnlyAGlobalNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "cowsay-from-global-nixpkgs"

  describe "When passing option combinations" $ do
    it "should print a message saying no package is specified when no argument is supplied" $
      shellifyWithArgs ""
        `shouldReturnSubstring`
      "without any packages specified"

    it "should complain if no package is specified" $
      shellifyWithArgs "nipkgs#cowsay"
        `shouldReturnSubstring`
      "without any packages specified"

    it "should show help text when requested" $ do
      shellifyWithArgs "-h"
          `shouldReturnSubstring` "USAGE:"
      shellifyWithArgs "--help"
          `shouldReturnSubstring` "USAGE:"
    it "should show the version number when requested" $ do
      shellifyWithArgs "--version"
          `shouldReturnSubstring` "Shellify 0."

    it "should not support -p with shell" $ do
      shellifyWithArgs "shell -p cowsay"
         `shouldBe`
        Left "-p not supported with new style commands"
      shellifyWithArgs "shell nixpkgs#python --packages foo nixpkgs#cowsay"
         `shouldBe`
        Left "--packages not supported with new style commands"

    it "should allow a simple command to be specified with a package" $
      theOptions "-p python --command cowsay"
        `shouldBe`
      Right def{packages=["python"], command=Just "cowsay"}

    it "should allow a simple command to be specified before a package" $
      theOptions "--run cowsay -p python"
        `shouldBe`
      Right def{packages=["python"], command=Just "cowsay"}

    it "should allow a simple command to be specified before and after a package" $
      theOptions "-p cowsay --command cowsay -p python"
        `shouldBe`
      Right def{packages=[ "cowsay", "python" ], command=Just "cowsay"}

    it "Should fail if command has no argument" $ do
      shellifyWithArgs "--command -p python"
          `shouldReturnSubstring` "Argument missing to switch"
      shellifyWithArgs "--command"
          `shouldReturnSubstring` "Argument missing to switch"

    it "should be able to specify one program to install after other arguments" $
      "foo -p python"
        `shouldResultInPackages`
      [ "python" ]

    it "should support multiple packages passed to -p" $
      "-p python cowsay"
        `shouldResultInPackages`
      [ "cowsay", "python" ]

    it "should only accept packages up to the next switch" $
      "-p python --arg x 2"
        `shouldResultInPackages`
      [ "python" ]

    it "should support multiple adjacent -p switches" $
      "-p python -p cowsay"
        `shouldResultInPackages`
      [ "python", "cowsay" ]

    it "should support separated -p switches" $
      "-p cowsay --foo -p python"
        `shouldResultInPackages`
      [ "cowsay", "python" ]

    it "should support long switches" $
      "--packages cowsay"
        `shouldResultInPackages`
      [ "cowsay" ]

    it "should support new shell commands" $
      theOptions "shell nixpkgs#python nixpkgs#cowsay"
        `shouldBe`
      Right def{packages=[ "nixpkgs#python", "nixpkgs#cowsay" ], generateFlake=True}

  describe "when one buildInputs is required from an unknown source" $
    shellifyWithArgs "--with-flake shell foo#cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "inputs-from-unknown-source"

  describe "when two buildInputs are required from two sources" $
    shellifyWithArgs "--with-flake shell foo#cowsay nixpkgs#python"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "inputs-from-know-and-unknown-sources"

  describe "when using 2 simple buildInputs from one source" $
    shellifyWithArgs "shell --with-flake python cowsay"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "two-nixpkgs-inputs"

  describe "when pulling from 2 different known sources" $
    shellifyWithArgs "shell nixpkgs#python blender-bin#blender_3_5"
      `shouldReturnShellAndFlakeTextDefinedBy`
    "inputs-from-different-registries"

  describe "when working with 2 nixkgs buildInputs" $
      shellifyWithArgs "shell nixpkgs#python nixpkgs#cowsay"
        `shouldReturnShellTextDefinedBy`
      "two-nixpkgs-inputs"

  describe "when working with multiple repos for known and unknown sources" $
      shellifyWithArgs "shell nixpkgs#python foo#cowsay"
        `shouldReturnShellTextDefinedBy`
      "multiple-repository-sources"

  describe "when a command is specified" $
      shellifyWithArgs "-p python -p cowsay --command cowsay"
        `shouldReturnShellTextDefinedBy`
      "two-build-inputs-and-command"
