{ custompkgs, pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  buildInputs = [
    custompkgs.cowsay
  ];

}
