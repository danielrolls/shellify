import Data.Either (isLeft)
import Data.Text (Text())
import Lib.Shellify
import Test.Hspec (Expectation(), hspec, it, shouldBe, shouldSatisfy)

shouldResultInPackages :: [Text] -> [Text] -> Expectation
shouldResultInPackages stringInput packages = options "foo" stringInput `shouldBe` Right def{packages=packages}

main :: IO ()
main = hspec $ do 
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

