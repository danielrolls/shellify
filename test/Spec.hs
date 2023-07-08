import Prelude hiding (readFile, last, putStrLn, reverse, tail)
import Data.Bool (bool)
import Data.Either (isLeft, isRight)
import Data.Maybe (fromJust)
import Data.Text (Text(), last, reverse, tail)
import Data.Text.IO (readFile, putStrLn)
import Test.Hspec (describe, Expectation(), hspec, it, shouldBe, shouldContain, shouldReturn, shouldSatisfy)
import Test.Hspec.Core.Spec (SpecM)

import Options
import Shellify
import TemplateGeneration
import TestConstants

main :: IO ()
main =
 do hspec $ do

     describe "When passing option combinations" $ do
       it "should allow a simple command to be specified with a package" $
         options "nix-shellify" ["-p", "python", "--command", "cowsay"]
           `shouldBe`
         Right def{packages=["python"], command=Just "cowsay"}

       it "should allow a simple command to be specified before a package" $
         options "nix-shellify" ["--run", "cowsay", "-p", "python" ]
           `shouldBe`
         Right def{packages=["python"], command=Just "cowsay"}

       it "should allow a simple command to be specified before and after a package" $
         options "nix-shellify" ["-p", "cowsay", "--command", "cowsay", "-p", "python" ]
           `shouldBe`
         Right def{packages=[ "cowsay", "python" ], command=Just "cowsay"}

       it "Should fail if command has no argument" $
         options "nix-shellify" ["--command", "-p", "python" ] `shouldSatisfy` isLeft

       it "should be able to specify one program to install after other arguments" $
         [ "nix-shellify", "foo", "-p", "python" ]
           `shouldResultInPackages`
         [ "python" ]

       it "should support multiple packages passes to -p" $
         [ "nix-shellify", "-p", "python", "cowsay" ]
           `shouldResultInPackages`
         [ "cowsay", "python" ]

       it "should only accept packages up to the next switch" $
         [ "nix-shellify", "-p", "python", "--arg", "x", "2" ]
           `shouldResultInPackages`
         [ "python" ]

       it "should support multiple adjacent -p switches" $
         [ "nix-shellify", "-p", "python", "-p", "cowsay"]
           `shouldResultInPackages`
         [ "python", "cowsay" ]

       it "should support separated -p switches" $
         [ "nix-shellify", "-p", "cowsay", "--foo", "-p", "python"]
           `shouldResultInPackages`
         [ "cowsay", "python" ]

       it "should support long switches" $
         [ "nix-shellify", "--packages", "cowsay" ]
           `shouldResultInPackages`
         [ "cowsay" ]

       it "should support new shell commands" $
         [ "nix-shellify", "shell", "nixpkgs#python", "nixpkgs#cowsay" ]
           `shouldResultInPackages`
         [ "nixpkgs#python", "nixpkgs#cowsay" ]

       it "should not support -p with shell" $
         options "nix-shellify" [ "shell", "-p", "cowsay" ]
           `shouldSatisfy`
         isLeft

       it "should not support -p switches when using new command style" $
         options  "nix-shellify" [ "shell", "nixpkgs#python", "-p", "foo", "nixpkgs#cowsay"]
           `shouldSatisfy`
         isLeft

     describe "when two buildInputs are required from two sources" $
       ["nix-shellify", "--with-flake", "shell", "foo#cowsay", "nixpkgs#python"]
         `shouldReturnShellAndFlakeTextDefinedBy`
       "inputs-from-know-and-unknown-sources"

     describe "when using 2 simple buildInputs from one source" $
       ["nix-shellify", "shell", "--with-flake", "python", "cowsay"]
         `shouldReturnShellAndFlakeTextDefinedBy`
       "two-nixpkgs-inputs"

     describe "when pulling from 2 different known sources" $
       ["nix-shellify", "shell", "--with-flake", "nixpkgs#python", "blender-bin#blender_3_5"]
         `shouldReturnShellAndFlakeTextDefinedBy`
       "inputs-from-different-registries"

     describe "when a command is specified" $
       it "should produce the expected shell.nix" $
         def{packages=["python", "cowsay"], command=Just "cowsay"}
           `shouldReturnShellTextOf`
         "two-build-inputs-and-command"

     describe "when working with 2 nixkgs buildInputs" $
       it "should produce the expected shell.nix" $
         def{packages=["nixpkgs#python", "nixpkgs#cowsay"]}
           `shouldReturnShellTextOf`
	 "two-nixpkgs-inputs"

     describe "when working with multiple repos for known and unknown sources" $
       it "should produce the expected shell.nix" $
         def{packages=["nixpkgs#python", "foo#cowsay"]}
           `shouldReturnShellTextOf`
         "multiple-repository-sources"

shouldReturnShellAndFlakeTextDefinedBy :: [Text] -> String -> SpecM () ()
shouldReturnShellAndFlakeTextDefinedBy (par:parms) expectedOutput =
     it "should produce the expected shell.nix and flake.nix" $
       do expShell <- readNixTemplate (shellFile expectedOutput)
          expFlake <- readNixTemplate (flakeFile expectedOutput)
          parseOptionsAndCalculateExpectedFiles db par parms
            `shouldBe`
           Right [("shell.nix", expShell),("flake.nix", expFlake)]

shouldResultInPackages :: [Text] -> [Text] -> Expectation
shouldResultInPackages (par:parms) packages =
     options par parms
       `shouldBe`
     Right def{packages=packages}

shouldReturnShellTextOf :: Options -> FilePath -> IO ()
shouldReturnShellTextOf input expectedOutputFile =
      readNixTemplate (shellFile expectedOutputFile) >>= shouldBe (generateShellDotNixText input)

shouldReturnFlakeTextOf :: Options -> FilePath -> IO ()
shouldReturnFlakeTextOf input expectedOutputFile =
      readNixTemplate (flakeFile expectedOutputFile) >>= shouldBe (fromJust (generateFlakeText db input))

readNixTemplate :: FilePath -> IO Text
readNixTemplate fileName =
    stripTrailingNewline <$> readFile ( "test/outputs/" <> fileName)
    where stripTrailingNewline f = bool id stripLastChar (lastCharIsNewline f) f
          lastCharIsNewline = (== '\n') . last
          stripLastChar = reverse . tail . reverse

flakeFile = (<> "-flake.nix")
shellFile = (<> "-shell.nix")
