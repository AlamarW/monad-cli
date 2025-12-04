#!/usr/bin/env bash
set -e

echo "======================================"
echo "Installing Haskell Development Tools"
echo "======================================"

# Install GHCup
if ! command -v ghcup &> /dev/null; then
    echo "Installing GHCup..."
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
else
    echo "GHCup already installed"
fi

# Source GHCup environment
export PATH="$HOME/.ghcup/bin:$PATH"

echo "Installing GHC (recommended version)..."
ghcup install ghc recommended
ghcup set ghc recommended

echo "Installing Stack (latest)..."
ghcup install stack latest

echo ""
echo "======================================"
echo "Setup complete!"
echo "======================================"
echo ""
echo "Please restart your shell or run:"
echo "  source ~/.ghcup/env"
echo ""
echo "Then verify installation:"
echo "  ghc --version"
echo "  stack --version"
