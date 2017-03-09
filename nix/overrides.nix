{ cabal2nixResult, testFun ? (x: x), self, super }:
let build = path: (testFun (self.callPackage (cabal2nixResult path) {}));
in {
  servant-render = build ../servant-render;
  servant-render-client = build ../servant-render-client;
  servant-render-server = build ../servant-render-server;

  # Example
  common = build ../example/common;
  frontend = build ../example/frontend;
  backend = build ../example/backend;
}
