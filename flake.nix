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
                shell.nativeBuildInputs =
                  let
                    wasm-dummy-liblibdl = pkgs.runCommand "liblibdl"
                      {
                        nativeBuildInputs = [ pkgs.pkgsCross.wasi32.buildPackages.llvmPackages.clang ];
                      }
                      ''
                        mkdir -p $out/lib
                        echo 'void __liblibdl_stub(void) {}' | wasm32-unknown-wasi-cc -shared -x c - -o $out/lib/liblibdl.so 2>/dev/null
                      '';
                  in
                  [
                    (pkgs.writeShellScriptBin "wasm32-unknown-wasi-cabal" ''
                      LD_LIBRARY_PATH="${wasm-dummy-liblibdl}/lib''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}" \
                      NIX_LDFLAGS=$(echo "$NIX_LDFLAGS" | tr ' ' '\n' | grep -v 'libffi-[0-9]' | tr '\n' ' ') \
                      NIX_LDFLAGS_FOR_TARGET=$(echo "$NIX_LDFLAGS_FOR_TARGET" | tr ' ' '\n' | grep -v 'libffi-[0-9]' | tr '\n' ' ') \
                      exec cabal \
                        --with-ghc=wasm32-unknown-wasi-ghc \
                        --with-compiler=wasm32-unknown-wasi-ghc \
                        --with-ghc-pkg=wasm32-unknown-wasi-ghc-pkg \
                        --with-hsc2hs=wasm32-unknown-wasi-hsc2hs \
                        $(builtin type -P "wasm32-unknown-wasi-pkg-config" &> /dev/null && echo "--with-pkg-config=wasm32-unknown-wasi-pkg-config") \
                        "$@"
                    '')
                  ];
                shell.tools.cabal = "latest";
                shell.tools.haskell-language-server = {
                  src = inputs.hls-2-13;
                  sha256map = {
                    "https://github.com/snowleopard/alga"."d4e43fb42db05413459fb2df493361d5a666588a" = "0s1mlnl64wj7pkg3iipv5bb4syy3bhxwqzqv93zqlvkyfn64015i";
                  };
                };
                shell.withHoogle = false;
                shell.shellHook =
                  let
                    ws = pkgs.stdenv.mkDerivation {
                      pname = "ws";
                      version = "8.18.0";
                      src = pkgs.fetchurl {
                        url = "https://registry.npmjs.org/ws/-/ws-8.18.0.tgz";
                        hash = "sha256-oIIh8oUUcslEygF42JqYiMf0P72ZmK/Ip+xv5BfEyiA=";
                      };
                      installPhase = ''
                        mkdir -p $out/lib/node_modules/ws
                        cp -r . $out/lib/node_modules/ws
                      '';
                    };
                  in
                  ''
                    export NODE_PATH="${ws}/lib/node_modules''${NODE_PATH:+:$NODE_PATH}"
                  '';
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
      in
      pkgs.myHaskellProject.flake { });
}
