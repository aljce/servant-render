{ compiler ? "ghcjs", package, test ? "true" }:
let bootstrap = import <nixpkgs> {};
    reflex-platform-commit = builtins.fromJSON (builtins.readFile ./reflex-platform.json);
    reflex-platform-src = bootstrap.fetchFromGitHub {
      owner = "reflex-frp";
      repo  = "reflex-platform";
      inherit (reflex-platform-commit) rev sha256;
    };
    reflex-platform = import reflex-platform-src {};
    parseBool = str: with builtins;
      let json = fromJSON str; in if isBool json then json else throw "parseBool: ${str} is not a bool";
    overrides = (builtins.getAttr compiler reflex-platform).override {
      overrides = self: super: with reflex-platform; import ./overrides.nix {
        inherit cabal2nixResult self super;
        testFun = pkg: lib.overrideCabal pkg (drv: { doCheck = parseBool test; });
      };
    };
    drv = builtins.getAttr package overrides;
in
if reflex-platform.nixpkgs.lib.inNixShell then
  drv.env
else
  drv
