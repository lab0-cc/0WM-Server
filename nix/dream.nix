{
  lib,
  fetchFromGitHub,
  makeOpamRepo,
}:
lib.makeExtensible (self: {
  src = fetchFromGitHub {
    owner = "bensmrs";
    repo = "dream";
    rev = "5126308698188454ad89fc1e6e6e5269da7845ae";
    hash = "sha256-LSlBMn4J2CxFOsDNLPUggKIHFUQz2eP4VIpXRPRt9Sc=";
  };

  repo = makeOpamRepo self.src;
})
