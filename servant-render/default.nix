{ compiler ? "ghcjs", test ? "true" }:
import ../nix/local.nix { inherit compiler test; package = "servant-render"; }
