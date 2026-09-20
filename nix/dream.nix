{
  lib,
  fetchFromGitHub,
  makeOpamRepo,
}:
lib.makeExtensible (self: {
  src = fetchFromGitHub {
    owner = "camlworks";
    repo = "dream";
    rev = "4718cb47264178d6d2181e0fcc2e8a8ff2170b5a";
    hash = "sha256-aSfQdN7jxIGx4GtTe63tOSnn0WgsIhYvMPiUuavftWA=";
  };

  repo = makeOpamRepo self.src;
})
