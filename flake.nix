# basically from https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html#scaffolding
{
  description = "Basic Haskell flake";
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix/hkm/ghc914X";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2511";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.hls-2-13 = { url = "github:haskell/haskell-language-server/2.13.0.0"; flake = false; };
  outputs = inputs@{ self, nixpkgs, flake-utils, haskell-nix, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-darwin" ] (system:
      let
        overlays = [
          haskell-nix.overlay
          (final: prev: {
            myHaskellProject =
              final.haskell-nix.hix.project {
                src = ./.;
                compiler-nix-name = "ghc9141";
                evalSystem = "x86_64-linux";
                crossPlatforms = p:
                  pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64
                    ([
                      # p.ghcjs
                      p.wasi32
                      # TODO not currently available in caches, and takes a long time to build from source
                      # p.mingwW64
                    ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux
                      [
                        # p.musl64
                        # p.aarch64-multiplatform
                      ]
                    );
                shell.tools.cabal = "latest";
                shell.tools.haskell-language-server.src = inputs.hls-2-13;
                shell.withHoogle = false;
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
      in
      pkgs.myHaskellProject.flake { });
}
