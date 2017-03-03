{ compiler ? "ghcjs", test ? "true" }:
with (import ./../.. { inherit compiler test; });
let drv = overrides.frontend;
in
if reflex-platform.nixpkgs.lib.inNixShell then
  reflex-platform.workOn overrides drv
else
  drv
