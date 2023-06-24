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
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShells.default = import ./shell.nix { inherit blender-bin pkgs; };
        }
      );
}
