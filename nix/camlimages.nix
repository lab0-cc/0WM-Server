{
  lib,
  fetchFromGitLab,
  makeOpamRepo,
}:
lib.makeExtensible (self: {
  src = fetchFromGitLab {
    owner = "camlspotter";
    repo = "camlimages";
    rev = "ef4caba407b738e98763001a4c85b3a5db19a739";
    hash = "sha256-7SRHOydUr5ZyFO1dz/qtGMU/M8cPqZyVohsK6EJLufQ=";
  };

  repo = makeOpamRepo self.src;
})
