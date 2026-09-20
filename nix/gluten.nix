{
  lib,
  fetchFromGitHub,
  makeOpamRepo,
}:
lib.makeExtensible (self: {
  src = fetchFromGitHub {
    owner = "bensmrs";
    repo = "gluten";
    rev = "668d961cf6edafecfd130b9b6b0ab01c3d2a4242";
    hash = "sha256-eJpbugus1PRZ6R/38cEr3L5GSN0ItXbfSP2tsHfsP/Y=";
  };

  repo = makeOpamRepo self.src;
})
