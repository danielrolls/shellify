{
  description = "shellify";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: 

    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in {

      packages.x86_64-linux.default = (pkgs.haskell.lib.overrideCabal (
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
      }).overrideAttrs (old: {
        installPhase = old.installPhase + ''  
          ln -s $out/bin/shellify $out/bin/nix-shellify
        '';
        buildInputs = old.buildInputs ++ [ pkgs.nix ];
      });
    };
}
