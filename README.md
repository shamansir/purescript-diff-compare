# purescript-diff-compare

Extension for comparing large amounts of text in PureScript, e.g. in tests

## Development with Nix Flakes

This project uses Nix flakes for reproducible development environments with:
- PureScript 0.15.15 or newer
- Spago 0.93 or newer
- Node.js

### Quick Start

```bash
# Enter development shell
nix develop

# Run the project with spago
spago run

# Or run directly with nix
nix run

# Or run with the spago-run app
nix run .#spago-run
```

### Available Commands

Inside the development shell (`nix develop`):
- `spago run` - Run the application
- `spago build` - Build the project
- `spago test` - Run tests

### Building

```bash
# Build the project with Nix
nix build

# The output will be in ./result/bin/purescript-diff-compare
./result/bin/purescript-diff-compare
```
