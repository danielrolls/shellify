module ShellifyTemplate where

import Text.RawString.QQ (r)

shellifyTemplate = [r|{ $parameters;separator=', '$ }:

pkgs.mkShell {

  buildInputs = [
    $build_inputs;separator='
    '$
  ];
$if(shell_hook)$

  shellHook = ''
    $shell_hook$
  '';
$endif$
}
|]
