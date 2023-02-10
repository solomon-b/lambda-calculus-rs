{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;
    crane.url = github:ipetkov/crane;
    pre-commit-hooks.url = github:cachix/pre-commit-hooks.nix;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { nixpkgs, crane, pre-commit-hooks, flake-utils, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        craneLib = crane.lib.${system};
        src = ./.;
        cargoArtifacts = craneLib.buildDepsOnly {
          inherit src;
        };
        book-clippy = craneLib.cargoClippy {
          inherit cargoArtifacts src;
          cargoClippyExtraArgs = "-- --deny warnings";
        };
        book = craneLib.buildPackage {
          inherit cargoArtifacts src;
          nativeBuildInputs = with pkgs; [
          ];
        };
        book-coverage = craneLib.cargoTarpaulin {
          inherit cargoArtifacts src;
        };
      in
      {
        defaultPackage = book;
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs;
            [
              rust-analyzer
              rnix-lsp
              rustfmt
              rustc
              cargo
            ];
        };
      });
}
