import Test.Hspec (describe, hspec, it, shouldBe)

import Options
import TestHelpers


main = hspec $ do

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
