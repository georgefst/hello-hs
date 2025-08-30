# basically from https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html#scaffolding
{
  description = "Basic Haskell flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            myHaskellProject =
              final.haskell-nix.hix.project {
                src = ./.;
                compiler-nix-name = "ghc9122";
                evalSystem = "x86_64-linux";
                crossPlatforms = p:
                  pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64
                    ([
                      p.mingwW64
                      p.ghcjs
                    ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux
                      [
                        p.musl64
                        p.aarch64-multiplatform
                      ]
                    );
                shell.tools.cabal = "latest";
                shell.tools.hlint = "latest";
                shell.tools.haskell-language-server = "latest";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      in
      pkgs.myHaskellProject.flake { });
}
