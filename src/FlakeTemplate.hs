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
        let pkgs = nixpkgs.legacyPackages.\${system}; in
        {
          devShells.default = import ./shell.nix { inherit $repo_vars;separator=' '$; };
        }
      );
}|]
