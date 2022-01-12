# This shell is suitable for building the IDE, and entering the FHS environment
# needed to run the Engine binary that is downloaded by default. To build an
# Engine from source, use shell.nix

{
 pkgs ? import <nixpkgs> { overlays = [ (import (fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz")) ]; }
}:

let
  lib = pkgs.lib;
  buildNodeJs = pkgs.callPackage <nixpkgs/pkgs/development/web/nodejs/nodejs.nix> { python = pkgs.python3; };

  nodejsVersion = "16.13.2";

  nodejs = buildNodeJs {
    enableNpm = true;
    version = nodejsVersion;
    sha256 = "sha256-mLHeH/kqKSuT0rLJO8KphlZkez0MDVYjBp9PgEeotKA=";
  };

  rust-bin = pkgs.rust-bin.nightly."2021-11-29".default.override {
    targets = [ "wasm32-unknown-unknown" ];
  };

  graal = pkgs.graalvm11-ce;

  wasm-pack_0_9 = with pkgs; callPackage ./nix/wasm-pack_0_9 {
    inherit (darwin.apple_sdk.frameworks) Security;
    libressl = libressl_3_2;
  };

in (pkgs.buildFHSUserEnv {
  name = "enso-env";
  targetPkgs = pkgs: (with pkgs; [
    # IDE
    rust-bin nodejs cargo-watch pkgconfig openssl.dev wasm-pack_0_9
    # Engine: runtime deps (FHS)
    zlib clang bintools
  ]);
}).env
