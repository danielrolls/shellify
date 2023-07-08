{ blender-bin, pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  buildInputs = [
    blender-bin.blender_3_5
    pkgs.python
  ];

}
