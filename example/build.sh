#!/usr/bin/env bash
set -e
cd frontend
nix-build --option extra-binary-caches https://nixcache.reflex-frp.org --argstr compiler ghcjs
cd ../backend
nix-build --option extra-binary-caches https://nixcache.reflex-frp.org
./result/bin/backend
