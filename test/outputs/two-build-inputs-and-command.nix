{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  buildInputs = [
    pkgs.cowsay
    pkgs.python
  ];

  shellHook = ''
    cowsay
  '';

}
