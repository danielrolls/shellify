{ foo, pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  buildInputs = [
    foo.cowsay
    pkgs.python
  ];

}
