{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage {
    pname = "haskell-debugger-extension";
    version = "0.13.1";
    src = ./.;

    npmDepsHash = "sha256-y236GuKdV8JsudIl+nfMu5CR1S7FTpv2KMt6eXJZZCs=";

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
