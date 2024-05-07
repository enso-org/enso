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
              sha256 = "sha256-o/MRwGYjLPyD1zZQe3LX0dOynwRJpVygfF9+vSnqTOc=";
            };
          in
          pkgs.mkShell {
            packages = with pkgs; [
              # === TypeScript dependencies ===
              nodejs_20 # should match the Node.JS version of the lambdas
              corepack
              # === Electron ===
              electron
              # === node-gyp dependencies ===
              python3
              gnumake
              # === WASM parser dependencies ===
              rust
              wasm-pack
              # Java and SBT omitted for now
            ];

            shellHook = ''
              # `sccache` can be used to speed up compile times for Rust crates.
              # `~/.cargo/bin/sccache` is provided by `cargo install sccache`.
              # `~/.cargo/bin` must be in the `PATH` for the binary to be accessible.
              export PATH=$HOME/.cargo/bin:$PATH
            '';
          });
    };
}
