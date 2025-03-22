{-# LANGUAGE TemplateHaskell #-}
module Options (Options(..), OutputForm(..), Package(), Packages(Packages), parseCommandLine, setPackages) where

import Constants (hlDesc, noPackagesError)

import Control.Lens.Combinators (makeLenses, makePrisms)
import Data.Default (Default(def))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Set (Set(), fromList)
import Data.Text (Text())
import Data.Version (showVersion)
import Options.Applicative ((<**>), Parser, ParserResult(Success, Failure, CompletionInvoked), argument, command, defaultPrefs, execParserPure, fullDesc, header, help, helper, hidden, info, long, metavar, option, optional, progDesc, short, simpleVersioner, some, str, strOption, switch)
import Paths_shellify (version)

data OutputForm = Traditional
                | Flake
     deriving (Eq, Show)

newtype Packages = Packages (Set Package)
        deriving (Eq, Monoid, Semigroup, Show)

type Package = Text

makePrisms ''Packages
setPackages = Packages . fromList

opts = info (commandParser <**> simpleVersioner (showVersion version)
                           <**> helper) $
            fullDesc
         <> progDesc hlDesc
         <> header "Quickly generate shell.nix files once you have a working shell"

commandParser :: Parser CommandLineOptions
commandParser = CommandLineOptions
     <$> optional (some (option str (
            long "packages"
         <> short 'p'
         <> metavar "PACKAGE"
         <> help "Packages to install in the shell.nix file. This option can be used multiple times to specify multiple packages"
         )))
     <*> optional (option str (
            long "command"
         <> long "run"
         <> short 'c'
         <> metavar "COMMAND"
         <> help "Command to run on initial shell startup"
         ))
     <*> switch (
            long "with-flake"
         <> help "When using the -p option to specify packages, use this switch to have a flake.nix created in addition to a shell.nix. This is recommended to ensure the versions of dependencies are kept for reproducibility and so that shells are cached to load faster."
         )
     <*> switch (
            long "allow-local-pinned-registries-to-be-prioritized"
         <> help "Pinned local repoisitory URLs are usually taken last when looking for URLs for generated flake.nix files. This is usually desired. If you do however want to see these pinned entries in the flake file as specified in your registry, then set this flag."
         )
     <*> optional (some (option str (
           long "arg"
        <> long "argstr"
        <> hidden
        )))
     <*> optional (some (argument str (metavar "shell PACKAGES...")))

data Options = Options {
    _packages :: !Packages
  , _command :: !(Maybe Text)
  , _outputForm :: !OutputForm
  , _prioritiseLocalPinnedSystem :: !Bool
} deriving (Eq, Show)

data CommandLineOptions = CommandLineOptions {
    __packages :: !(Maybe [Text])
  , __command :: !(Maybe Text)
  , __withFlake :: !Bool
  , __prioritiseLocalPinnedSystem :: Bool
  , __discard :: !(Maybe [String])
  , __shellPackages :: Maybe [Package]
} deriving (Show)

makeLenses ''Options

instance Default Options where
  def = Options {
    _packages = mempty,
    _command = Nothing,
    _outputForm = Traditional,
    _prioritiseLocalPinnedSystem = False
  }

parseCommandLine :: [String] -> Either Text (ParserResult Options)
parseCommandLine =
    (\case
      Success res -> fmap Success (parseCommandLineOptions res)
      Failure failure -> Right $ Failure failure
      CompletionInvoked f -> Right $ CompletionInvoked f)
      . execParserPure defaultPrefs opts . fixupRequest
  where parseCommandLineOptions :: CommandLineOptions -> Either Text Options
        parseCommandLineOptions originalParsedOptions =
          if _packages transformedOptions == mempty then
            Left noPackagesError
          else
            Right transformedOptions
          where transformedOptions =
                  (Options <$> setPackages . ((++) <$> fromMaybe mempty . __packages
                                                   <*> shellArgs . __shellPackages)
                           <*> __command
                           <*> \case
                             f | __withFlake f -> Flake
                               | (hasShellArg . __shellPackages) f -> Flake
                             _ | otherwise -> Traditional
                           <*> __prioritiseLocalPinnedSystem) originalParsedOptions
                hasShellArg (Just ("shell":_)) = True
                hasShellArg _ = False
                shellArgs (Just ("shell": rst)) = rst
                shellArgs _ = []
        fixupRequest (a : b : c : d) | (a == "-p" || a == "--packages")
                                     && isNotASwitch b
                                     && isNotASwitch c =
                                         a : b : fixupRequest ("-p" : c : d)
          where isNotASwitch = not . isPrefixOf "-"
        fixupRequest (a : b) = a : fixupRequest b
        fixupRequest [] = []
