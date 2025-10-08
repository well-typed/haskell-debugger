{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage {
    pname = "haskell-debugger-extension";
    version = "0.8.0";
    src = ./.;

    npmDepsHash = "sha256-rvPlvEsFygi/EYh0vcOBDAC4Sf5nzJIfaN8HjdsVXE0=";

    nativeBuildInputs = [
      pkgs.nodejs pkgs.vsce pkgs.cacert
      pkgs.esbuild pkgs.pkg-config
    ];

    buildInputs = [
      pkgs.libsecret.dev # needed for pkg-config for some npm thing
    ];

    buildPhase = ''
      npm run package
    '';

    installPhase = ''
      mkdir -p $out
      mv *.vsix $out/
    '';
}
