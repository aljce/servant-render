{ compiler ? "ghc", test ? "true" }:
import ../../nix/local.nix { inherit compiler test; package = "common"; }
