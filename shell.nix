# This is the shell for building the IDE + Engine. It is not suitable for
# running the Engine binary that the build scripts will use as a backend by
# default; for that, use shell-fhs.nix

{
 pkgs ? import <nixpkgs> { overlays = [ (import (fetchTarball "https://github.com/oxalica/rust-overlay/archive/master.tar.gz")) ]; }
}:

let
  lib = pkgs.lib;

  # IDE (and common with Engine)

  buildNodeJs = pkgs.callPackage <nixpkgs/pkgs/development/web/nodejs/nodejs.nix> { python = pkgs.python3; };
  nodejs = buildNodeJs {
    enableNpm = true;
    version = "16.13.2";
    sha256 = "sha256-mLHeH/kqKSuT0rLJO8KphlZkez0MDVYjBp9PgEeotKA=";
  };

  rust-bin = pkgs.rust-bin.nightly."2021-11-29".default.override { targets = [ "wasm32-unknown-unknown" ]; };

  # Engine

  graalvm_11_1_0 = with pkgs; recurseIntoAttrs (callPackage ./nix/graalvm-ce {
    inherit (darwin.apple_sdk.frameworks) Foundation;
  }).graalvm11-ce;

  sbt_graal = pkgs.sbt.override { jre = graalvm_11_1_0; };
in pkgs.mkShell {
  packages = with pkgs; [
    # IDE (and common)
    rust-bin nodejs cargo-watch pkgconfig openssl.dev wasm-pack
    # Engine
    sbt_graal maven flatbuffers_1_12 graalvm_11_1_0
  ];
}

