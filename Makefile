.PHONY: help setup lint format format-check clean dev build serve pdf
.DEFAULT_GOAL := help

# Project variables
PROJECT_NAME := miso-numeron
PORT := 8080

help: ## Show this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

setup: ## Initial project setup (download dependencies)
	@if command -v nix > /dev/null; then \
		echo "📦 Setting up project dependencies..."; \
		nix develop .#wasm --command echo "✅ Dependencies cached and ready"; \
		echo "📝 Run 'make dev' to enter development shell"; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

# Main development commands (WASM by default)
dev: ## Enter WASM development shell
	@if command -v nix > /dev/null; then \
		nix develop .#wasm; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

build: ## Build WASM for deployment
	@if command -v nix > /dev/null; then \
		echo "🚀 Building WASM..."; \
		nix develop .#wasm --command sh -c 'wasm32-wasi-cabal update && \
			wasm32-wasi-cabal build --allow-newer && \
			WASM_PATH=$$(wasm32-wasi-cabal list-bin miso-numeron --allow-newer 2>/dev/null | tail -1) && \
			mkdir -p docs && \
			cp -v "$$WASM_PATH" docs/app.wasm && \
			$$(wasm32-wasi-ghc --print-libdir)/post-link.mjs --input "$$WASM_PATH" --output docs/ghc_wasm_jsffi.js && \
			echo "✅ WASM build complete: docs/"'; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

serve: ## Serve WASM build locally (port 8080)
	@if [ -f "docs/app.wasm" ]; then \
		echo "🌐 Serving at http://localhost:8080"; \
		cd docs && python3 -m http.server 8080; \
	else \
		echo "❌ No WASM build found. Run 'make build' first"; \
	fi

lint: ## Lint code with hlint (in Nix shell)
	@if command -v nix > /dev/null; then \
		nix develop --command hlint src/; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

format: ## Format code with ormolu (in Nix shell)
	@if command -v nix > /dev/null; then \
		nix develop --command sh -c 'find src -name "*.hs" -exec ormolu --mode inplace {} \;'; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

format-check: ## Check if code is formatted (in Nix shell)
	@if command -v nix > /dev/null; then \
		nix develop --command sh -c 'find src -name "*.hs" -exec ormolu --mode check {} \;'; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

pdf: ## Build algorithm analysis PDF documentation
	@if command -v nix > /dev/null; then \
		if [ -f "algorithm.tex" ]; then \
			echo "📄 Building algorithm.pdf..."; \
			nix develop .#wasm --command tectonic algorithm.tex && \
			echo "✅ PDF built: algorithm.pdf"; \
		else \
			echo "❌ algorithm.tex not found"; \
			exit 1; \
		fi \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

clean: ## Clean build artifacts and WASM files
	@echo "🧹 Cleaning build artifacts..."
	@rm -rf docs/app.wasm docs/ghc_wasm_jsffi.js dist-newstyle .ghc.environment.*
	@if command -v nix > /dev/null; then \
		nix develop .#wasm --command wasm32-wasi-cabal clean 2>/dev/null || true; \
	fi
	@echo "✅ Cleaned"

.SILENT: help