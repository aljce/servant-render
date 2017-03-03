let bootstrap = import <nixpkgs> {};
    reflex-platform-commit = builtins.fromJSON (builtins.readFile ./reflex-platform.json);
    reflex-platform-src = bootstrap.fetchFromGitHub {
      owner = "reflex-frp";
      repo  = "reflex-platform";
      inherit (reflex-platform-commit) rev sha256;
    };
in
{ compiler ? "ghcjs", test ? "true", reflex-platform ? (import reflex-platform-src {}) }:
let parseBool = str: with builtins;
      let json = fromJSON str; in if isBool json then json else throw "parseBool: ${str} is not a bool";
in {
  inherit reflex-platform;
  overrides = (builtins.getAttr compiler reflex-platform).override {
    overrides = self: super:
      with reflex-platform; with self;
      let testFun = if parseBool test then x: x else lib.dontCheck;
          build   = path: testFun (callPackage (cabal2nixResult path) {});
      in {
        servant-render = build ./servant-render;
        servant-render-client = build ./servant-render-client;
        servant-render-server = build ./servant-render-server;

        common = build ./example/common;
        frontend = build ./example/frontend;
        backend = build ./example/backend;
      };
  };
}
