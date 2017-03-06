{ mkDerivation, base, common, reflex-dom, servant-render
, servant-render-client, stdenv
}:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base common reflex-dom servant-render servant-render-client
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  description = "Frontend for the servant-render example project";
  license = stdenv.lib.licenses.mit;
}
