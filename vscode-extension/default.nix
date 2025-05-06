{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage {
    pname = "haskell-debugger-extension";
    version = "0.1.0.0";
    src = ./.;

    npmDepsHash = "sha256-ZNrEHzO2/uO9kE2LXBqvqiM8ZMF1AZ1//loCQ6zHzGc=";

    nativeBuildInputs = [
      pkgs.nodejs pkgs.vsce pkgs.cacert pkgs.nodePackages.rimraf
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
