module FlakeTemplate where

import Text.RawString.QQ (r)


flakeTemplate = [r|{
  description = "my project description";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";
    $repo_inputs;separator='
    '$

  };

  outputs = { self, $repos;separator=', '$, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let $pkgs_decls;separator='
            '$
        in
        {
          devShells.default = import ./shell.nix { $shell_args;separator=' '$ };
        }
      );
}
|]
