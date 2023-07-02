module Constants where

import Data.Text (Text())
import Text.RawString.QQ (r)

helpText :: Text -> Text
helpText progName = "USAGE: " <> progName <> [r| -p [PACKAGES] 
       |] <> progName <> [r| [--with-flakes] [PACKAGES]

Pass nix-shell arguments to nix-shellify to have it generate a shell.nix in
the current directory. You can then just run nix shell or nix-shell in that
directory to have those directories in your environment. To run nix commands
you must first install Nix.

Options

    -p / --packages
    Specify packages for nix-shell compatability

    --command / --run
    Command to run after creating the shell

    --with-flake
    When using the command in a flake-like style use this switch to have a
    flake.nix created in addition to a shell.nix. Highly recommended to ensure
    the versions of dependencies are kept for reproducibility and so that
    shells are cached to load faster.
|]

noPackagesError = [r|I can't write out a shell file without any packages specified.
Try 'nix-shellify --help' for more information.|] :: Text

pkgsDecl var repo = var <> [r| = if builtins.hasAttr "packages" |] <> repo <> [r| then |] <> repo <> [r|.packages.${system} else ( if builtins.hasAttr "legacyPackages" |] <> repo <> [r| then |] <> repo <> [r|.legacyPackages.${system} else |] <> repo <> [r|);|]
