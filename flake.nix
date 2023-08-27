{
  description = "shellify";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:

    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {

      packages.x86_64-linux.default = pkgs.haskell.lib.overrideCabal (
        pkgs.haskellPackages.developPackage {
            root = ./.;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
                [ cabal-install
                  hlint
                  haskell-language-server
                ]);
        }
      ) {
        enableSeparateDataOutput = false;
      };
    };
}
