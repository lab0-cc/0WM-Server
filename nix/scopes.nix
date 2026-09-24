{
  lib,
  buildOpamProject',
  opam-repository,

  camlimages,
  cf,
  dream,
  gluten,
  irmin,
  query,
  src,
  zwm-server,
}:
lib.makeExtensible (self: {
  # Turn the project into a scope that contains all the packages.
  default = buildOpamProject' {
    repos = [
      opam-repository
      camlimages.repo
      cf.repo
      dream.repo
      gluten.repo
      irmin.repo
    ];
    resolveArgs.with-test = true;
  } src query;

  # Override the main package inside the scope.
  main = self.default.overrideScope zwm-server.overlay;
})
