{
  lib,
  fetchFromGitHub,
  makeOpamRepo,
}:
lib.makeExtensible (self: {
  src = fetchFromGitHub {
    owner = "bensmrs";
    repo = "gendarme";
    rev = "9cd85a965a029615c586a4b37a44b2eb29e3fedf";
    hash = "sha256-2M/T9TiSQkSRq3Dm+T7/zVQ1m8NsAh4KCusAojPkXgM=";
    fetchSubmodules = true;
  };

  repo = makeOpamRepo self.src;
})
