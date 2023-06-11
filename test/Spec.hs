import Prelude hiding (readFile, last, putStrLn, reverse, tail)
import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Data.Text (Text(), last, pack, reverse, tail)
import Data.Text.IO (readFile, putStrLn)
import Shellify
import Test.Hspec (describe, Expectation(), hspec, it, shouldBe, shouldReturn, shouldSatisfy)


main :: IO ()
main = hspec $ do
     describe "when two buildInputs are required from two sources" $ do
       let input = def{packages=[ "foo#cowsay", "nixpkgs#python" ], generateFlake=True}
       it "should produce the expected shell.nix" $
         input `shouldReturnShellTextOf` "two-build-inputs-from-two-sources"
       it "should produce the exected flake.nix" $
         input `shouldReturnFlakeTextOf` "flake-for-two-inputs"

     describe "when using 2 simple buildInputs from one source" $ do
       let input = def{packages=["python", "cowsay"], generateFlake=True}
       it "should produce the expected shell.nix" $
         input `shouldReturnShellTextOf` "simple-two-build-inputs"
       it "should produce the expected flake.nix" $
         input `shouldReturnFlakeTextOf` "flake-for-a-nixpkgs-input"

     it "should produce the expected shell.nix for 2 nixpkgs buildInputs" $
       def{packages=["nixpkgs#python", "nixpkgs#cowsay"]}
         `shouldReturnShellTextOf`
       "simple-two-build-inputs"

     it "should produce the expected shell.nix for multiple repo sources" $
       def{packages=["nixpkgs#python", "foo#cowsay"]}
         `shouldReturnShellTextOf`
       "multiple-repository-sources"

     it "should allow a simple command to be specified with a package" $
       options "nix-shellify" ["-p", "python", "--command", "cowsay"]
         `shouldBe`
       Right def{packages=["python"], command=Just "cowsay"}

     it "should allow a simple command to be specified before a package" $
       options "nix-shellify" ["--command", "cowsay", "-p", "python" ]
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

     it "should not support -p switches when using new command style" $
       options  "nix-shellify" [ "shell", "nixpkgs#python", "-p", "foo", "nixpkgs#cowsay"]
         `shouldSatisfy`
       isLeft

     it "should produce the expected shell.nix when a command is specified" $
       def{packages=["python", "cowsay"], command=Just "cowsay"}
         `shouldReturnShellTextOf`
       "two-build-inputs-and-command"

shouldResultInPackages :: [Text] -> [Text] -> Expectation
shouldResultInPackages (par:parms) packages =
     options par parms
       `shouldBe`
     Right def{packages=packages}

shouldReturnShellTextOf :: Options -> FilePath -> IO ()
shouldReturnShellTextOf input expectedOutputFile =
      readNixTemplate expectedOutputFile >>= shouldBe (generateShellDotNixText input)

shouldReturnFlakeTextOf :: Options -> FilePath -> IO ()
shouldReturnFlakeTextOf input expectedOutputFile =
      readNixTemplate expectedOutputFile >>= shouldBe (fromJust (generateFlakeText input))

readNixTemplate fileName =
    stripTrailingNewline <$> readFile ( "test/outputs/" <> fileName <> ".nix")
    where stripTrailingNewline f = if last f == '\n'
                                   then reverse . tail . reverse $ f
                                   else f
