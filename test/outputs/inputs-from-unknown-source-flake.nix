{
  description = "my project description";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";
    foo.url = "PLEASE ENTER input here";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  };

  outputs = { self, foo, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let fooPkgs = if builtins.hasAttr "packages" foo then foo.packages.${system} else ( if builtins.hasAttr "legacyPackages" foo then foo.legacyPackages.${system} else foo);
            nixpkgsPkgs = if builtins.hasAttr "packages" nixpkgs then nixpkgs.packages.${system} else ( if builtins.hasAttr "legacyPackages" nixpkgs then nixpkgs.legacyPackages.${system} else nixpkgs);
        in
        {
          devShells.default = import ./shell.nix { foo=fooPkgs; pkgs=nixpkgsPkgs; };
        }
      );
}
