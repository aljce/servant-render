{ compiler ? "ghc", test ? "true" }:
with (import ./.. { inherit compiler test; });
let drv = overrides.servant-render-server;
in
if reflex-platform.nixpkgs.lib.inNixShell then
  reflex-platform.workOn overrides drv
else
  drv
