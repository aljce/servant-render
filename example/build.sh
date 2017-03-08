#!/usr/bin/env bash
set -e
cd frontend
nix-build
cd ../backend
nix-build
./result/bin/backend
