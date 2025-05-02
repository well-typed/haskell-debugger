{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
    pname = "haskell-debugger-extension";
    version = "0.1.0";
    src = ./.;
    nativeBuildInputs = [pkgs.nodejs pkgs.yarn pkgs.vsce pkgs.cacert];
    buildPhase = ''
      export HOME=$TMPDIR
      touch $HOME/.yarnrc
      yarn install --frozen-lockfile
      yarn run build
      yarn run package
    '';

    installPhase = ''
      mkdir -p $out
      mv *.vsix $out/
    '';
}
