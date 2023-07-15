module Options (Package(..), Options(..), def, Packages, options) where
import Prelude hiding (takeWhile, writeFile)

import Constants
import FlakeTemplate
import ShellifyTemplate

import Control.Applicative ((<|>))
import Control.Arrow ((+++))
import Control.Monad (when)
import Data.Bool (bool)
import Data.Default.Class (Default(def))
import Data.List (find, sort)
import Data.Maybe (fromMaybe)
import Data.Text (isInfixOf, isPrefixOf, pack, replace, splitOn, stripPrefix, takeWhile, Text(), unpack)
import Data.Text.IO (hPutStrLn, writeFile)
import qualified Data.Text.IO as Text
import GHC.IO.Exception (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesPathExist)
import System.Exit (exitWith)
import System.IO (stderr)
import Text.StringTemplate (newSTMP, render, setAttribute, StringTemplate)

type Package = Text
type Packages = [ Package ]
data Options = Options {
    packages :: Packages
  , command :: Maybe Text
  , generateFlake :: Bool
}

instance Default Options where
  def = Options [] Nothing False

instance Eq Options where
  a == b =  isEqual command
         && isEqual (sort . packages)
         && isEqual generateFlake
    where isEqual f = f a == f b

data OptionsParser = OptionsParser [Text] -- remainingOptions
                                   (Either Text (Options -> Options)) -- result

options :: Text -> [Text] -> Either Text Options
options progName args =
  let optionsHandler | hasShellArg args = newStyleOption
                     | otherwise = oldStyleOption
      shellArgFilter | hasShellArg args = filter (/= "shell")
                     | otherwise = id
      optionsCaller f = worker 
       where worker (OptionsParser [] t) = t
             worker (OptionsParser (hd:tl) res) =
               let (OptionsParser newRemaining newRes) = f hd tl
               in worker $ OptionsParser newRemaining ((.) <$> newRes <*> res)

      screenForNoPackages (Right opts) | packages opts == [] = Left noPackagesError
      screenForNoPackages anyThingElse = anyThingElse
      initialArgumentsToParse = shellArgFilter args
      initialModifier = Right $ if hasShellArg args then setFlakeGeneration else id
      initialOptionParser = OptionsParser initialArgumentsToParse initialModifier
  in screenForNoPackages $ (\f -> f def) <$> optionsCaller optionsHandler initialOptionParser

  where oldStyleOption :: Text -> [Text] -> OptionsParser
        oldStyleOption "-p" = handlePackageSwitch
        oldStyleOption "--packages" = handlePackageSwitch
        oldStyleOption opt = baseOption opt
        newStyleOption "-p" = returnError "-p not supported with new style commands"
        newStyleOption "--packages" = returnError "--packages not supported with new style commands"
        newStyleOption arg | isSwitch arg = baseOption arg
                           | otherwise = transformOptionsWith $ appendPackages [arg]
        baseOption :: Text -> [Text] -> OptionsParser
        baseOption "-h" = returnError $ helpText progName
        baseOption "--help" = returnError $ helpText progName
        baseOption "--command" = handleCommandSwitch
        baseOption "--run" = handleCommandSwitch
        baseOption "--with-flake" = transformOptionsWith setFlakeGeneration
        baseOption _ = transformOptionsWith id
        --doNothing = transformOptionsWith id
        transformOptionsWith fun wds = OptionsParser wds (Right fun)
        handlePackageSwitch wds = let (pkgs, remainingOptions) = consumePackageArgs wds
                                  in transformOptionsWith (appendPackages pkgs) remainingOptions
        handleCommandSwitch (hd:tl) | isSwitch hd
                                    = returnError "Argument missing to switch" tl
                                    | otherwise
                                    = transformOptionsWith (setCommand hd) tl
        handleCommandSwitch [] = returnError "Argument missing to switch" []

        appendPackages ps opts = opts{packages=ps ++ packages opts}
        setCommand cmd opts = opts{command=Just cmd}
        setFlakeGeneration opts = opts{generateFlake=True}
        returnError errorText remaining = OptionsParser remaining $ Left errorText

consumePackageArgs :: [Text] -> (Packages, [Text])
consumePackageArgs = worker []
  where worker pkgs [] = (pkgs, [])
        worker pkgs options@(hd:_) | isSwitch hd
                                   = (pkgs, options)
        worker pkgs (hd:tl) = worker (hd:pkgs) tl

hasShellArg [] = False
hasShellArg ("shell":_) = True
hasShellArg (hd:tl) | isSwitch hd = hasShellArg tl
                    | otherwise = False

isSwitch = isPrefixOf "-"
