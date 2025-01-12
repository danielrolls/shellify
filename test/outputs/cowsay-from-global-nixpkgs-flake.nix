{
  description = "my project description";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-global-registry";

  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let nixpkgsPkgs = if builtins.hasAttr "packages" nixpkgs then nixpkgs.packages.${system} else ( if builtins.hasAttr "legacyPackages" nixpkgs then nixpkgs.legacyPackages.${system} else nixpkgs);
        in
        {
          devShells.default = import ./shell.nix { pkgs=nixpkgsPkgs; };
        }
      );
}
