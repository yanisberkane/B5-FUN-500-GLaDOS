#!/bin/bash

# Install Haskell
sudo apt-get update -qy
sudo apt-get install haskell-platform -y

# Install Stack
curl -sSL https://get.haskellstack.org/ | sh

# Verify installation
stack --version
ghc --version
