{-# LANGUAGE TemplateHaskell #-}
module Options (Package(..), Options(..), OutputForm(..), def, Packages(Packages), options) where

import Constants
import FlakeTemplate
import ShellifyTemplate

import Control.Lens.Combinators (makeLenses, makePrisms, set, over, view)
import Data.Default (Default(def))
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (isPrefixOf, pack, Text())
import Data.Version (showVersion)
import Paths_shellify (version)

data OutputForm = Traditional
                | Flake
     deriving (Eq, Show)

newtype Packages = Packages [ Package ] deriving Show

type Package = Text

instance Eq Packages where
  Packages a == Packages b = sort a == sort b

makePrisms ''Packages

data Options = Options {
    _packages :: !Packages
  , _command :: !(Maybe Text)
  , _outputForm :: !OutputForm
  , _prioritiseLocalPinnedSystem :: !Bool
} deriving (Eq, Show)

makeLenses ''Options

packageList = view (packages . _Packages)

data OptionsParser = OptionsParser [Text] -- | remainingOptions
                                   (Either Text (Options -> Options)) -- | result

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

      screenForNoPackages (Right opts) | null (packageList opts) = Left noPackagesError
      screenForNoPackages anyThingElse = anyThingElse
      initialArgumentsToParse = shellArgFilter args
      initialModifier = Right $ if hasShellArg args then setFlakeGeneration else id
      initialOptionParser = OptionsParser initialArgumentsToParse initialModifier
  in screenForNoPackages $ ($ def) <$> optionsCaller optionsHandler initialOptionParser

  where oldStyleOption :: Text -> [Text] -> OptionsParser
        oldStyleOption "-p" = handlePackageSwitch
        oldStyleOption "--packages" = handlePackageSwitch
        oldStyleOption opt = baseOption opt
        newStyleOption "-p" = returnError "-p and --packages are not supported with new style commands"
        newStyleOption "--packages" = returnError "-p and --packages are not supported with new style commands"
        newStyleOption "--allow-local-pinned-registries-to-be-prioritized" = transformOptionsWith $ set prioritiseLocalPinnedSystem True
        newStyleOption arg | isSwitch arg = baseOption arg
                           | otherwise = transformOptionsWith $ appendPackages [arg]
        baseOption :: Text -> [Text] -> OptionsParser
        baseOption "-h" = returnError $ helpText progName
        baseOption "--help" = returnError $ helpText progName
        baseOption "--version" = returnError $ "Shellify " <> pack ( showVersion version)
        baseOption "--command" = handleCommandSwitch
        baseOption "--run" = handleCommandSwitch
        baseOption "--with-flake" = transformOptionsWith setFlakeGeneration
        baseOption _ = transformOptionsWith id
        transformOptionsWith fun wds = OptionsParser wds (Right fun)
        handlePackageSwitch wds = let (pkgs, remainingOptions) = consumePackageArgs wds
                                  in transformOptionsWith (appendPackages pkgs) remainingOptions
        handleCommandSwitch (hd:tl) | isSwitch hd
                                    = returnError "Argument missing to switch" tl
                                    | otherwise
                                    = transformOptionsWith (set Options.command (Just hd)) tl
        handleCommandSwitch [] = returnError "Argument missing to switch" []

        appendPackages = over (packages. _Packages) . (++)
        setFlakeGeneration = set outputForm Flake
        returnError errorText remaining = OptionsParser remaining $ Left errorText

consumePackageArgs :: [Text] -> ([Package], [Text])
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

instance Default Options where
  def = Options (Packages []) Nothing Traditional False
