{ foo, pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  buildInputs = [
    pkgs.python
    foo.cowsay
  ];

}
