{ compiler ? "ghcjs", test ? "true" }:
import ../../nix/default.nix { inherit compiler test; package = "frontend"; }
