import Data.Either (isLeft)
import Data.Text (Text())
import Lib.Shellify(options, Options(Options))
import Test.Hspec (Expectation(), hspec, it, shouldBe, shouldSatisfy)

shouldResultInPackages :: [Text] -> [Text] -> Expectation
shouldResultInPackages stringInput packages = options stringInput `shouldBe` Right (Options packages Nothing)

main :: IO ()
main = hspec $
     do it "should be able to specify one program to install" $
          ["-p", "python"] `shouldResultInPackages` [ "python" ]

        it "should allow a simple command to be specified with a package" $
          options ["-p", "python", "--command", "cowsay"] `shouldBe` Right(Options ["python"] (Just "cowsay"))

        it "should allow a simple command to be specified before a package" $
          options ["--command", "cowsay", "-p", "python" ] `shouldBe` Right(Options ["python"] (Just "cowsay"))

        it "should allow a simple command to be specified before and after a package" $
          options ["-p", "cowsay", "--command", "cowsay", "-p", "python" ] `shouldBe` Right(Options ["python", "cowsay" ] (Just "cowsay"))

        it "Should fail if command has no argument" $
          options ["--command", "-p", "python" ] `shouldSatisfy` isLeft

        it "should be able to specify one program to install after other arguments" $
          ["foo", "-p", "python"] `shouldResultInPackages` [ "python"]

        it "should specify no packages when no -p is passed" $
          ["foo"] `shouldResultInPackages` []

        it "should support multiple packages passes to -p" $
          ["-p", "python", "cowsay"] `shouldResultInPackages` [ "cowsay", "python" ]

        it "should only accept packages up to the next switch" $
          ["-p", "python", "--arg", "x", "2"] `shouldResultInPackages` [ "python" ]

        it "should support multiple adjacent -p switches" $
          ["-p", "python", "-p", "cowsay"] `shouldResultInPackages` [ "cowsay", "python" ]

        it "should support separated -p switches" $
          ["-p", "cowsay", "--foo", "-p", "python"] `shouldResultInPackages` [ "python", "cowsay" ]

        it "should support long switches" $
          ["--packages", "cowsay"] `shouldResultInPackages` [ "cowsay" ]

