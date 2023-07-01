module Constants where

import Data.Text (Text())
import Text.RawString.QQ (r)

helpText :: Text -> Text
helpText progName = "USAGE: " <> progName <> [r| -p [PACKAGES] 

Pass nix-shell arguments to nix-shellify to have it generate a shell.nix in
the current directory. You can then just run nix-shell in that directory to
have those directories in your environment. To run nix-shell you must first
install Nix.|]

noPackagesError = [r|I can't write out a shell file without any packages specified.
Try 'nix-shellify --help' for more information.|] :: Text

pkgsDecl var repo = var <> [r| = if builtins.hasAttr "packages" |] <> repo <> [r| then |] <> repo <> [r|.packages.${system} else ( if builtins.hasAttr "legacyPackages" |] <> repo <> [r| then |] <> repo <> [r|.legacyPackages.${system} else |] <> repo <> [r|);|]
