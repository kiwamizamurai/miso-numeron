# Miso Numeron

A number guessing game built with Haskell and Miso, deployed as WebAssembly.

## Game Rules

Guess a 4-digit number (each digit 0-9 is unique):
- **EAT**: Correct digit in correct position
- **BITE**: Correct digit in wrong position

## Prerequisites

- **Nix** (required): https://nixos.org/download.html

## Quick Start

```bash
# Enter WASM development environment
make dev

# Build WASM for deployment
make build

# Test locally
make serve
# Open http://localhost:8080
```

## Development

All development is done through Nix for reproducible builds:

```bash
# Available commands
make help          # Show all commands
make build         # Build WASM
make serve         # Serve locally
make lint          # Run hlint
make format        # Format code
make clean         # Clean artifacts
```

## Deployment

The `docs/` folder contains the WASM build artifacts for GitHub Pages deployment.
