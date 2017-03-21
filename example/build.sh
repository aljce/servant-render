#!/usr/bin/env bash
set -e
cd frontend
export TMPDIR=/home/kyle/tmp
nix-build --option binary-caches https://nixcache.reflex-frp.org
cd ../backend
nix-build --option binary-caches https://nixcache.reflex-frp.org
./result/bin/backend
