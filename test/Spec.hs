import Data.Default (Default(def))
import Test.Hspec (describe, hspec, it, shouldBe, specify)

import Options
import TestHelpers

main = hspec $ do

  describe "When passing option combinations" $ do

    it "prints a message saying no package is specified when no argument is supplied" $
      shellifyWithArgs ""
        `shouldReturnSubstring`
      "without any packages specified"

    it "should complain if no package is specified" $
      shellifyWithArgs "nipkgs#cowsay"
        `shouldReturnSubstring`
      "without any packages specified"

    it "shows the version number when requested" $ do
      shellifyWithArgs "--version"
          `shouldReturnSubstring` "0."

    describe "When using the --command option" $ do

      it "produces a shell with a shell hook" $
          shellifyWithArgs "-p python -p cowsay --command cowsay"
            `shouldReturnShellTextDefinedBy`
          "two-build-inputs-and-command"

      it "allows a command to be specified with a package" $
        theOptions "-p python --command cowsay"
          `shouldBe`
        Right def{_packages=Packages ["python"], _command=Just "cowsay"}

      it "allows a command to be specified before a package" $
        theOptions "--run cowsay -p python"
          `shouldBe`
        Right def{_packages=Packages ["python"], _command=Just "cowsay"}

      it "allows a command to be specified before and after a package" $
        theOptions "-p cowsay --command cowsay -p python"
          `shouldBe`
        Right def{_packages=Packages [ "cowsay", "python" ], _command=Just "cowsay"}

      it "fails if command has no argument" $
        shellifyWithArgs "--command"
            `shouldReturnSubstring` "expects an argument"

    it "supports specifying one program to install after other arguments" $
      "foo -p python"
        `shouldResultInPackages`
      [ "python" ]

    it "supports multiple packages passed to -p" $
      "-p python cowsay"
        `shouldResultInPackages`
      [ "cowsay", "python" ]

    it "only accepts packages up to the next switch" $
      "-p python --arg x 2"
        `shouldResultInPackages`
      [ "python" ]

    it "supports multiple adjacent -p switches" $
      "-p python -p cowsay"
        `shouldResultInPackages`
      [ "python", "cowsay" ]

    it "supports separated -p switches" $
      "-p cowsay --arg 4 cowsay -p python"
        `shouldResultInPackages`
      [ "cowsay", "python" ]

    it "supports long switches" $
      "--packages cowsay"
        `shouldResultInPackages`
      [ "cowsay" ]

    it "supports new shell commands" $
      theOptions "shell nixpkgs#python nixpkgs#cowsay"
        `shouldBe`
      Right def{_packages=Packages [ "nixpkgs#python", "nixpkgs#cowsay" ], _outputForm=Flake}

    it "supports the --with-flake option" $
      theOptions "--with-flake -p python -p cowsay"
        `shouldBe`
      Right def{_packages=Packages [ "python", "cowsay" ], _outputForm=Flake}

  describe "When dealing with multiple source repositories it should produce the correct output files for" $ do

    specify "one buildInput required from an unknown source" $
      shellifyWithArgs "--with-flake shell foo#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "inputs-from-unknown-source"

    specify "two buildInputs required from two sources" $
      shellifyWithArgs "--with-flake shell foo#cowsay nixpkgs#python"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "inputs-from-know-and-unknown-sources"

    specify "2 buildInputs from one source" $
      shellifyWithArgs "shell --with-flake python cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "two-nixpkgs-inputs"

    specify "pulling from 2 different known sources" $
      shellifyWithArgs "shell nixpkgs#python blender-bin#blender_3_5"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "inputs-from-different-registries"

    specify "2 nixpkgs buildInputs" $
        shellifyWithArgs "shell nixpkgs#python nixpkgs#cowsay"
          `shouldReturnShellTextDefinedBy`
        "two-nixpkgs-inputs"

    specify "multiple repos for known and unknown sources" $
        shellifyWithArgs "shell nixpkgs#python foo#cowsay"
          `shouldReturnShellTextDefinedBy`
        "multiple-repository-sources"

  describe "Where repository URLs need retrieving from the Nix Registry" $ do

    it "falls through to global where system is local and global is available" $
      whereALocalSystemAndFlakeGlobalNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-global-nixpkgs"

    it "prefers system to global where system is local and always-take-from-system-registry is specified" $
      whereALocalSystemAndFlakeGlobalNixpkgsExistsShellifyWithArgs "--allow-local-pinned-registries-to-be-prioritized shell nixpkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-local-nixpkgs"

    it "uses the local url where system has a local URL and no user or global repository is available" $
      whereALocalSystemNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-local-nixpkgs"

    it "uses the user registry when no system or global alternative exists" $
      whereAUserNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-user-nixpkgs"

    it "uses the system custom registry when a system and then global alternative are defined" $
      whereASystemAndGlobalCustomPkgsExistsShellifyWithArgs "shell custompkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-system-custompkgs"

    it "uses the system custom registry when a global and then system alternative are defined" $
      whereAGlobalAndSystemCustomPkgsExistsShellifyWithArgs "shell custompkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-system-custompkgs"

    it "uses the system nixpkgs when a system and then global alternative exist" $
      whereASystemAndGlobalNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-system-nixpkgs"

    it "uses the system nixpkgs when a global and then system alternative exist" $
      whereAGlobalAndSystemNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-system-nixpkgs"

    it "uses the global nixpkgs when no alternative exists" $
      whereOnlyAGlobalNixpkgsExistsShellifyWithArgs "shell nixpkgs#cowsay"
        `shouldReturnShellAndFlakeTextDefinedBy`
      "cowsay-from-global-nixpkgs"
