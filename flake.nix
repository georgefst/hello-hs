# basically from https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html#scaffolding
{
  description = "Basic Haskell flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            helloProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc924";
                # This adds `aarch64-unknown-linux-gnu-cabal` to the shell.
                shell.crossPlatforms = p: [ p.aarch64-multiplatform ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.helloProject.flake {
          # This adds support for `nix build .#aarch64-unknown-linux-gnu:hello-hs:exe:hello-hs`
          crossPlatforms = p: [ p.aarch64-multiplatform ];
        };
      in
      flake // {
        defaultPackage = flake.packages."hello-hs:exe:hello-hs";
      });
}
