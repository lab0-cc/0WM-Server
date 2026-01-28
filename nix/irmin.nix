{
  lib,
  fetchFromGitHub,
  makeOpamRepo,
}:
lib.makeExtensible (self: {
  src = fetchFromGitHub {
    owner = "mirage";
    repo = "irmin";
    rev = "7a09a06fff67bc4981faca36a332c51fc16e819e";
    hash = "sha256-bnmecOgf6wBjgxtlaDc4K5JF30tZNlIo5QkUFZG1ROY=";
  };

  repo = makeOpamRepo self.src;
})
