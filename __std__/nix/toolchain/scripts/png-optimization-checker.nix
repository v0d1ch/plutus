{ inputs, cell }:

let
  src = inputs.nixpkgs.lib.sourceFilesBySuffices
    (cell.library.gitignore-source inputs.self)
    [ ".png" ];
in

inputs.nixpkgs.runCommand "png-optimization-checker"
{
  buildInputs = [
    cell.packages.fix-png-optimization
    inputs.nixpkgs.diffutils
    inputs.nixpkgs.glibcLocales
  ];
}
  ''
    set +e
    cp -a ${src} orig
    cp -a ${src} png
    chmod -R +w png
    cd png
    fix-png-optimization
    cd ..
    diff --brief --recursive orig png > /dev/null
    EXIT_CODE=$?
    if [[ $EXIT_CODE != 0 ]]
    then
      mkdir -p $out/nix-support
      diff -ur orig png > $out/png.diff
      echo "file none $out/png.diff" > $out/nix-support/hydra-build-products
      echo "*** optipng found changes that need addressed first"
      echo "*** Please run fix-png-optimization and commit changes"
      echo "*** or apply the diff generated by hydra if you don't have nix."
      exit $EXIT_CODE
    else
      echo $EXIT_CODE > $out
    fi
  ''
