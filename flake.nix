# basically from https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html#scaffolding
{
  description = "Basic Haskell flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        crossPlatforms = p: [
          # Provides `nix build .#aarch64-unknown-linux-gnu:hello-hs:exe:hello-hs`.
          # And `aarch64-unknown-linux-gnu-cabal` etc. in the shell.
          p.musl64
        ];
        overlays = [
          haskellNix.overlay
          (final: prev: {
            myHaskellProject =
              final.haskell-nix.project' {
                src = ./.;
                compiler-nix-name = "ghc924";
                shell = { inherit crossPlatforms; };
              };
          })

          haskellNix.overlay
          (final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl {
            libevdev = prev.libevdev.overrideAttrs (_: { dontDisableStatic = true; });
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      in
      pkgs.myHaskellProject.flake { inherit crossPlatforms; });
}
