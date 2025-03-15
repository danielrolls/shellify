module Constants where

import Data.Text (Text())
import Text.RawString.QQ (r)

hlDesc :: String
hlDesc = [r|
  Pass nix-shell arguments to nix-shellify to have it generate a shell.nix in
  the current directory. You can then just run nix develop or nix-shell in that
  directory to have those packages in your environment. To run nix commands
  you must first install Nix.
|]

noPackagesError = [r|I can't write out a shell file without any packages specified.
Try 'nix-shellify --help' for more information.|] :: Text

pkgsDecl var repo = var <> [r| = if builtins.hasAttr "packages" |] <> repo <> [r| then |] <> repo <> [r|.packages.${system} else ( if builtins.hasAttr "legacyPackages" |] <> repo <> [r| then |] <> repo <> [r|.legacyPackages.${system} else |] <> repo <> [r|);|]
