import Prelude hiding (readFile)
import Data.Either (isLeft)
import Data.Text (Text())
import Data.Text.IO (readFile)
import Shellify
import Test.Hspec (Expectation(), hspec, it, shouldBe, shouldReturn, shouldSatisfy)

main :: IO ()
main = hspec $ do
     it "should produce the expected shell.nix for 2 simple buildInputs" $
       generateShellDotNixText def{packages=["python", "cowsay"]}
         `shouldReturnShellTextOf`
       "simple-two-build-inputs"

     it "should produce the expected shell.nix for 2 nixpkgs buildInputs" $
       generateShellDotNixText def{packages=["nixpkgs#python", "nixpkgs#cowsay"]}
         `shouldReturnShellTextOf`
       "simple-two-build-inputs"

     it "should produce the expected shell.nix for multiple repo sources" $
       generateShellDotNixText def{packages=["nixpkgs#python", "foo#cowsay"]}
         `shouldReturnShellTextOf`
       "multiple-repository-sources"

     it "should be able to specify one program to install" $
       ["nix-shellify", "-p", "python"]
         `shouldResultInPackages`
       [ "python" ]

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
       [ "foo", "-p", "python" ]
         `shouldResultInPackages`
       [ "python" ]

     it "should specify no packages when no -p is passed" $
       [ "foo" ]
         `shouldResultInPackages`
       [ ]

     it "should support multiple packages passes to -p" $
       [ "-p", "python", "cowsay" ]
         `shouldResultInPackages`
       [ "cowsay", "python" ]

     it "should only accept packages up to the next switch" $
       [ "-p", "python", "--arg", "x", "2" ]
         `shouldResultInPackages`
       [ "python" ]

     it "should support multiple adjacent -p switches" $
       [ "-p", "python", "-p", "cowsay"]
         `shouldResultInPackages`
       [ "python", "cowsay" ]

     it "should support separated -p switches" $
       [ "-p", "cowsay", "--foo", "-p", "python"]
         `shouldResultInPackages`
       [ "cowsay", "python" ]

     it "should support long switches" $
       [ "--packages", "cowsay" ]
         `shouldResultInPackages`
       [ "cowsay" ]

     it "should produce the expected shell.nix when a command is specified" $
       generateShellDotNixText def{packages=["python", "cowsay"], command=Just "cowsay"}
         `shouldReturnShellTextOf`
       "two-build-inputs-and-command"

shouldResultInPackages :: [Text] -> [Text] -> Expectation
shouldResultInPackages stringInput packages = options "foo" stringInput `shouldBe` Right def{packages=packages}

shouldReturnShellTextOf :: IO (Either Text Text) -> FilePath -> IO ()
shouldReturnShellTextOf actualGeneratedNixShell expectedOutputFile =
      readFile ( "test/outputs/" <> expectedOutputFile <> ".nix")
  >>= shouldReturn actualGeneratedNixShell . Right 

