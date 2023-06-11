{
  description = "my project description";

  inputs = {

    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    foo.url = "PLEASE ENTER input here";

  };

  outputs = { self, foo, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShells.default = import ./shell.nix { inherit foo pkgs; };
        }
      );
}