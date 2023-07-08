{
  description = "my project description";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";
    blender-bin.url = "github:edolstra/nix-warez?dir=blender";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  };

  outputs = { self, blender-bin, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let blender-binPkgs = if builtins.hasAttr "packages" blender-bin then blender-bin.packages.${system} else ( if builtins.hasAttr "legacyPackages" blender-bin then blender-bin.legacyPackages.${system} else blender-bin);
            nixpkgsPkgs = if builtins.hasAttr "packages" nixpkgs then nixpkgs.packages.${system} else ( if builtins.hasAttr "legacyPackages" nixpkgs then nixpkgs.legacyPackages.${system} else nixpkgs);
        in
        {
          devShells.default = import ./shell.nix { blender-bin=blender-binPkgs; pkgs=nixpkgsPkgs; };
        }
      );
}
