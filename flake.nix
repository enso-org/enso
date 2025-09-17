{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    fenix.url = github:nix-community/fenix;
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs2.url = "github:nixos/nixpkgs?rev=0feb4cf3d7931133c4e8e7a558e8153f13fe6b6a";
  };
  outputs = { self, nixpkgs, nixpkgs2, fenix }:
    let
      forAllSystems = with nixpkgs.lib; f: foldAttrs mergeAttrs { }
        (map (s: { ${s} = f s; }) systems.flakeExposed);
    in
    {
      devShell = forAllSystems
        (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
            pkgs2 = nixpkgs2.legacyPackages.${system};
            rust = fenix.packages.${system}.fromToolchainFile {
              dir = ./.;
              sha256 = "sha256-IeUO263mdpDxBzWTY7upaZqX+ODkuK1JLTHdR3ItlkY=";
            };
            isOnLinux = pkgs.lib.hasInfix "linux" system;
            rust-jni =
              if isOnLinux then with fenix.packages.${system}; combine [
                minimal.cargo
                minimal.rustc
                targets.x86_64-unknown-linux-musl.latest.rust-std
              ] else fenix.packages.${system}.minimal.toolchain;
            # https://github.com/NixOS/nixpkgs/blob/618c81f7b15d3e2dd73d9d413d9e7b13fbc9520f/pkgs/development/tools/build-managers/bazel/bazel_7/default.nix#L58
            defaultShellUtils = with pkgs; [
              bash
              coreutils
              diffutils
              file
              findutils
              gawk
              gnugrep
              gnupatch
              gnused
              gnutar
              gzip
              python3
              unzip
              which
              zip
              makeWrapper
            ];
            # https://github.com/NixOS/nixpkgs/blob/618c81f7b15d3e2dd73d9d413d9e7b13fbc9520f/pkgs/development/tools/build-managers/bazel/bazel_7/default.nix#L257
            defaultShellPath = pkgs.lib.makeBinPath defaultShellUtils;
            bazel = (pkgs2.bazel_8.overrideAttrs (self: super: {
              patches = super.patches ++ [
                (pkgs.substituteAll {
                  src = ./nix/patches/bazel_actions_path.patch;
                  actionsPathPatch = defaultShellPath;
                })
              ];
            }));
            pnpm-shim = pkgs.writeShellScriptBin "pnpm" ''
              set -euo pipefail
              PACKAGE_JSON=$(git rev-parse --show-toplevel)/package.json
              trap "sed -i 's#\"postinstall\": \"${bazel}/bin/bazel#\"postinstall\": \"bazel#' \"$PACKAGE_JSON\"" EXIT
              sed -i 's#"postinstall": "bazel#"postinstall": "${bazel}/bin/bazel#' "$PACKAGE_JSON"
              ${pkgs.corepack}/bin/pnpm "$@"
            '';
            rustup-shim = pkgs.writeShellScriptBin "rustup" ''
              case "$3" in
                x86_64-unknown-linux-musl)
                  echo 'Installing Nix Rust shims'
                  ln -sf ${rust-jni}/bin/rustc $out/bin/rustc
                  ln -sf ${rust-jni}/bin/cargo $out/bin/cargo
                  ;;
                *)
                  echo 'Uninstalling Nix Rust shims (if installed)'
                  rm -f $out/bin/{rustc,cargo}
                  ;;
              esac
            '';
          in
          pkgs.mkShell rec {
            buildInputs = with pkgs; [
              # === Bazel ===
              bazel
              # === Graal dependencies ===
              libxcrypt-legacy
              # === Rust dependencies ===
              openssl.dev
              pkg-config
            ] ++ (if !isOnLinux then [
              # === macOS-specific dependencies ===
              darwin.apple_sdk.frameworks.IOKit # Required by `enso-formatter`.
              darwin.apple_sdk.frameworks.Security # Required by `enso-formatter`.
            ] else [ ]);

            packages = with pkgs; [
              # === Shims (highest precedence) ===
              pnpm-shim
              rustup-shim
              # === TypeScript dependencies ===
              nodejs_22
              corepack
              # === Electron ===
              electron
              # === node-gyp dependencies ===
              python3
              gnumake
              # === WASM parser dependencies ===
              rust
            ];

            shellHook = ''
              # `sccache` can be used to speed up compile times for Rust crates.
              # `~/.cargo/bin/sccache` is provided by `cargo install sccache`.
              # `~/.cargo/bin` must be in the `PATH` for the binary to be accessible.
              export PATH=$HOME/.cargo/bin:$PATH
              export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
            '';
          });
    };
}
