{
  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    fenix.url = github:nix-community/fenix;
    fenix.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, nixpkgs, fenix }:
    let
      forAllSystems = with nixpkgs.lib; f: foldAttrs mergeAttrs { }
        (map (s: { ${s} = f s; }) systems.flakeExposed);
    in
    {
      devShell = forAllSystems
        (system:
          let
            pkgs = nixpkgs.legacyPackages.${system};
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
          in
          pkgs.mkShell rec {
            buildInputs = with pkgs; [
              # === Graal dependencies ===
              libxcrypt-legacy
            ];

            packages = with pkgs; [
              # === TypeScript dependencies ===
              nodejs_20
              corepack
              # === Electron ===
              electron
              # === node-gyp dependencies ===
              python3
              gnumake
              # === WASM parser dependencies ===
              rust
              wasm-pack
            ];

            shellHook = ''
              SHIMS_PATH=$HOME/.local/share/enso/nix-shims
              # `sccache` can be used to speed up compile times for Rust crates.
              # `~/.cargo/bin/sccache` is provided by `cargo install sccache`.
              # `~/.cargo/bin` must be in the `PATH` for the binary to be accessible.
              export PATH=$SHIMS_PATH:${rust.out}:$HOME/.cargo/bin:$PATH
              export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"

              # `rustup` shim
              mkdir -p $SHIMS_PATH
              cat <<END > $SHIMS_PATH/rustup
              if [ "\$3" = "x86_64-unknown-linux-musl" ]; then
                echo 'Installing Nix Rust shims'
                ln -s ${rust-jni.out}/bin/rustc $SHIMS_PATH
                ln -s ${rust-jni.out}/bin/cargo $SHIMS_PATH
              else
                echo 'Uninstalling Nix Rust shims (if installed)'
                rm -f $SHIMS_PATH/{rustc,cargo}
              fi
              END
              chmod +x $SHIMS_PATH/rustup
              # Uninstall shims if already installed
              $SHIMS_PATH/rustup
            '';
          });
    };
}
