{
  description = "my project description";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";
    custompkgs.url = "github:NixOS/custompkgs/custompkgs-system-registry";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  };

  outputs = { self, custompkgs, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let custompkgsPkgs = if builtins.hasAttr "packages" custompkgs then custompkgs.packages.${system} else ( if builtins.hasAttr "legacyPackages" custompkgs then custompkgs.legacyPackages.${system} else custompkgs);
            nixpkgsPkgs = if builtins.hasAttr "packages" nixpkgs then nixpkgs.packages.${system} else ( if builtins.hasAttr "legacyPackages" nixpkgs then nixpkgs.legacyPackages.${system} else nixpkgs);
        in
        {
          devShells.default = import ./shell.nix { custompkgs=custompkgsPkgs; pkgs=nixpkgsPkgs; };
        }
      );
}
