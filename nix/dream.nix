{
  lib,
  fetchFromGitHub,
  makeOpamRepo,
}:
lib.makeExtensible (self: {
  src = fetchFromGitHub {
    owner = "camlworks";
    repo = "dream";
    rev = "d076a45d8285452d898395f69c4c78cf0dcfb8f0";
    hash = "sha256-8xzSAKLt/dnRa21/sZ6BYIKZTWE/vvkI1GTLbdg6s2U=";
  };

  repo = makeOpamRepo self.src;
})
