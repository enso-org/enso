{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    nixpkgs2.url = "github:nixos/nixpkgs?rev=ebf9d4445d9e916239caa8d12a510e94a6d58a2f";
  };
  outputs =
    {
      self,
      nixpkgs,
      nixpkgs2,
      fenix,
    }:
    let
      forAllSystems =
        with nixpkgs.lib;
        f: foldAttrs mergeAttrs { } (map (s: { ${s} = f s; }) systems.flakeExposed);
    in
    {
      devShell = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pkgs2 = nixpkgs2.legacyPackages.${system};
          rust = fenix.packages.${system}.fromToolchainFile {
            dir = ./.;
            sha256 = "sha256-SJwZ8g0zF2WrKDVmHrVG3pD2RGoQeo24MEXnNx5FyuI=";
          };
          isOnLinux = pkgs.lib.hasInfix "linux" system;
          rust-jni =
            if isOnLinux then
              with fenix.packages.${system};
              combine [
                minimal.cargo
                minimal.rustc
                targets.x86_64-unknown-linux-musl.latest.rust-std
              ]
            else
              fenix.packages.${system}.minimal.toolchain;

          # https://github.com/NixOS/nixpkgs/blob/618c81f7b15d3e2dd73d9d413d9e7b13fbc9520f/pkgs/development/tools/build-managers/bazel/bazel_7/default.nix#L58
          defaultShellUtils = with pkgs2; [
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
          # First: override stdenv like upstream did.
          # NOTE [NP]: This workaround should not be necessary from Bazel 8.4.0 onwards.
          version = "8.2.1";
          bazelBase = pkgs2.bazel_8.override {
            inherit version;
            stdenv = if pkgs2.stdenv.cc.isClang then pkgs2.llvmPackages_17.stdenv else pkgs2.stdenv;
          };
          # https://github.com/NixOS/nixpkgs/blob/618c81f7b15d3e2dd73d9d413d9e7b13fbc9520f/pkgs/development/tools/build-managers/bazel/bazel_7/default.nix#L257
          defaultShellPath = pkgs2.lib.makeBinPath defaultShellUtils;
          bazel = (
            bazelBase.overrideAttrs (
              final: prev: {
                # Downgrade the version from 8.4.0 to 8.2.1 (needs to match our `.bazelversion`).
                inherit version;
                src = pkgs2.fetchzip {
                  url = "https://github.com/bazelbuild/bazel/releases/download/${version}/bazel-${version}-dist.zip";
                  hash = "sha256-aMvIadR0pXgdkz/8dzLfS79ubmYdwOLu4yECI06wvgI=";
                  stripRoot = false;
                };

                patches =
                  let
                    isDarwin = pkgs2.stdenv.isDarwin;
                    # Wrap the `codesign` binary required by Bazel on macOS so that `cctools` is
                    # present in the `PATH` and therefore the binary can find `codesign_allocate`.
                    sigtoolWrapped = pkgs2.runCommand "sigtool-wrapped" { buildInputs = [ pkgs2.makeWrapper ]; } ''
                      mkdir -p $out/bin
                      makeWrapper ${pkgs2.darwin.sigtool}/bin/codesign $out/bin/codesign \
                        --prefix PATH : ${pkgs2.darwin.cctools}/bin
                    '';
                  in
                  # Filter out upstream patches that are not compatible with our version.
                  # Drop the Darwin-only pathches only on Darwin; drop `deps_patches` everywhere.
                  (builtins.filter (
                    p:
                    let
                      matches = a: b: pkgs2.lib.hasSuffix a (baseNameOf (toString b));
                    in
                    !(matches "deps_patches.patch" p)
                    && (!isDarwin || !(matches "apple_cc_toolchain.patch" p || matches "xcode.patch" p))
                  ))
                    prev.patches
                  # Add our non-conditional patch replacements.
                  ++ [
                    ./nix/patches/deps_patches.patch
                  ]
                  # Add our Darwin-only replacements.
                  ++ pkgs2.lib.optionals isDarwin [
                    ./nix/patches/apple_cc_toolchain.patch
                    (pkgs2.replaceVars ./nix/patches/xcode.patch {
                      usrBinEnv = "${pkgs2.coreutils}/bin/env";
                      clangDarwin = "${pkgs2.stdenv.cc}/bin/clang";
                      # Replace the upstream `${darwin.sigtool}/bin/codesign` with an actual `codesign`.
                      # Necessary to properly sign binaries on macOS.
                      codesign = "${sigtoolWrapped}/bin/codesign";
                    })
                  ]
                  # Apply extra patch to fix the `$PATH` issues with `pnpm`.
                  ++ [
                    (pkgs2.replaceVars ./nix/patches/bazel_actions_path.patch {
                      actionsPathPatch = defaultShellPath;
                    })
                  ];
              }
            )
          );

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

          # Conditionally set the bazelrc only if the target system is Darwin (macOS).
          bazelrc =
            if pkgs.stdenv.isDarwin then
              pkgs.writeText ".bazelrc.local" ''
                # These flags are dynamically generated by the Darwin flake module.
                #
                # Add `try-import %workspace%/.bazelrc.local` to your .bazelrc to include these
                # flags when running Bazel in a nix environment. These are the libs and frameworks
                # used by darwin.

                build --@rules_rust//:extra_exec_rustc_flags=-L${pkgs.libiconv}/lib
              ''
            else
              "";
        in
        pkgs.mkShell rec {
          buildInputs =
            with pkgs;
            [
              # === Bazel ===
              bazel
              # === Graal dependencies ===
              libxcrypt-legacy
              # === Rust dependencies ===
              openssl.dev
              pkg-config
            ]
            ++ (
              if !isOnLinux then
                [
                  # === macOS-specific dependencies ===
                  libiconv # Required by `sysinfo` (via `ide_ci`).
                ]
              else
                [ ]
            );

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
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"

            if ! readlink "''${PWD}/.bazelrc.local" >/dev/null \
              || [[ $(readlink "''${PWD}/.bazelrc.local") != "${bazelrc}" ]]; then
              echo 1>&2 "Darwin: updating $PWD repository"
              [ -L .bazelrc.local ] && unlink .bazelrc.local

              if [ -e "''${PWD}/.bazelrc.local" ]; then
                echo 1>&2 "Darwin: WARNING: Refusing to install because of pre-existing .bazelrc.local"
                echo 1>&2 "  Remove the .bazelrc.local file and add .bazelrc.local to .gitignore."
              else
                ln -fs ${bazelrc} "''${PWD}/.bazelrc.local"
              fi
            fi
          '';
        }
      );
    };
}
