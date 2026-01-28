{
  lib,
  fetchFromGitHub,
  makeOpamRepo,
}:
lib.makeExtensible (self: {
  src = fetchFromGitHub {
    owner = "jetjinser";
    repo = "ocaml-cf";
    rev = "92099f5918ce0a32633d4030660943504f33cc86";
    hash = "sha256-qAHZ6t6PJ6SPRKZX0kQogodqQrv0EtHguOBt9391M4k=";
  };

  repo = makeOpamRepo self.src;
})
