{
  description = "Miso Numeron - A number guessing game built with Haskell and Miso";

  inputs = {
    # Pin to NixOS 24.05 stable for reproducibility
    nixpkgs.url = "github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798";
    flake-utils.url = "github:numtide/flake-utils";
    
    # WASM backend for GHC (following miso's approach)
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Haskell packages for development
        haskellPackages = pkgs.haskell.packages.ghc96;
        
        # Project package
        miso-numeron = haskellPackages.callCabal2nix "miso-numeron" ./. {};
        
        # Development tools
        devTools = with haskellPackages; [
          cabal-install
          ghcid
          hlint
          ormolu
          haskell-language-server
        ];
        
        # WASM build tools
        wasmTools = [
          pkgs.wabt           # wasm-opt, wasm-strip
          pkgs.binaryen       # Additional WASM tools
        ];
        
      in {
        # Package for regular GHC build
        packages = {
          default = miso-numeron;
          miso-numeron = miso-numeron;
        };
        
        # Development shells
        devShells = {
          # Default development shell (JSaddle)
          default = pkgs.mkShell {
            buildInputs = [
              haskellPackages.ghc
              pkgs.cabal-install
              pkgs.pkg-config
            ] ++ devTools ++ wasmTools ++ [
              pkgs.nodejs
              pkgs.nodePackages.http-server
            ];
            
            shellHook = ''
              echo "🎯 Miso Numeron Development Environment"
              echo ""
              echo "Available commands:"
              echo "  make build    - Build with GHC"
              echo "  make run      - Run development server (JSaddle)"
              echo "  make dev      - Run with auto-reload (ghcid)"
              echo ""
              echo "GHC version: $(ghc --version)"
              echo "Cabal version: $(cabal --version | head -1)"
            '';
          };
          
          # WASM development shell (following miso's approach)
          wasm = pkgs.mkShell {
            buildInputs = wasmTools ++ [
              ghc-wasm-meta.packages.${system}.all_9_12
              pkgs.cabal-install
              pkgs.nodejs
              pkgs.nodePackages.http-server
              # Development tools for code quality
              haskellPackages.hlint
              haskellPackages.ormolu
              haskellPackages.haskell-language-server
              # LaTeX compiler for documentation
              pkgs.tectonic
            ];
            
            shellHook = ''
              echo "🎯 Miso Numeron WASM Development Environment"
              echo ""
              echo "Available commands:"
              echo "  wasm32-wasi-cabal build  - Build WASM"
              echo "  make build         - Full WASM build and deployment"
              echo "  make serve         - Serve WASM locally"
              echo "  make lint          - Run hlint"
              echo "  make format        - Format with ormolu"
              echo "  make format-check  - Check formatting"
              echo "  make pdf           - Build algorithm.pdf documentation"
              echo ""
              echo "WASM GHC version: $(wasm32-wasi-ghc --version 2>/dev/null || echo 'Loading...')"
              
              # Helper functions like miso
              function build() {
                wasm32-wasi-cabal build $1
              }
              function clean() {
                wasm32-wasi-cabal clean
              }
              function update() {
                wasm32-wasi-cabal update
              }
            '';
          };
          
          # JavaScript backend shell (GHC JS backend)
          js = pkgs.mkShell {
            buildInputs = [
              pkgs.pkgsCross.ghcjs.haskell.packages.ghc96.ghc
              pkgs.cabal-install
              pkgs.nodejs
              pkgs.nodePackages.http-server
            ];
            
            shellHook = ''
              echo "🎯 Miso Numeron JavaScript Development Environment"
              echo ""
              echo "Available commands:"
              echo "  cabal build        - Build with GHC JS backend"
              echo "  make js-build      - Full JS build"
              echo "  make js-serve      - Serve the JS build"
              echo ""
              echo "GHC JS version: $(javascript-unknown-ghcjs-ghc --version 2>/dev/null || echo 'Loading...')"
              echo ""
              echo "To build for GitHub Pages:"
              echo "  1. cabal build"
              echo "  2. cp -r \$(cabal list-bin miso-numeron).jsexe ./docs"
              echo "  3. git add docs && git commit && git push"
            '';
          };
        };
        
        # Apps (executables)
        apps = {
          default = flake-utils.lib.mkApp {
            drv = miso-numeron;
          };
          
          # Development server
          dev-server = {
            type = "app";
            program = "${pkgs.writeShellScript "dev-server" ''
              ${haskellPackages.ghc}/bin/runghc \
                -package-env=- \
                -package miso \
                -package jsaddle-warp \
                src/Main.hs
            ''}";
          };
        };
      });
}