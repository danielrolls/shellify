import Test.Hspec

import Data.Text (Text())
import Lib.Shellify(options, Options(Options))

shouldResultInPackages :: [Text] -> [Text] -> Expectation
shouldResultInPackages stringInput packages = options stringInput `shouldBe` Options packages

main :: IO ()
main = hspec $ 
     do it "should be able to specify one program to install" $
          ["-p", "python"] `shouldResultInPackages` [ "python" ]

        it "should be able to specify one program to install after other arguments" $
          ["foo", "-p", "python"] `shouldResultInPackages` [ "python"]

        it "should specify no packages when no -p is passed" $
          ["foo"] `shouldResultInPackages` []

        it "should support multiple packages passes to -p" $
          ["-p", "python", "cowsay"] `shouldResultInPackages` [ "python", "cowsay" ]

        it "should only accept packages up to the next switch" $
          ["-p", "python", "--arg", "x", "2"] `shouldResultInPackages` [ "python" ]

        it "should support multiple adjacent -p switches" $
          ["-p", "python", "-p", "cowsay"] `shouldResultInPackages` [ "python", "cowsay" ]

        it "should support separated -p switches" $
          ["-p", "cowsay", "--foo", "-p", "python"] `shouldResultInPackages` [ "cowsay", "python" ]

        it "should support long switches" $
          ["--packages", "cowsay"] `shouldResultInPackages` [ "cowsay" ]



