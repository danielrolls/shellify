{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  buildInputs = [ pkgs.python pkgs.cowsay ];

}
