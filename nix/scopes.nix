{
  lib,
  buildOpamProject',
  opam-repository,

  gendarme,
  query,
  src,
  zwm-server,
}:
lib.makeExtensible (self: {
  # Turn the project into a scope that contains all the packages.
  default = buildOpamProject' {
    repos = [
      opam-repository
      gendarme.repo
    ];
    resolveArgs.with-test = true;
  } src query;

  # Override the main package inside the scope.
  main = self.default.overrideScope zwm-server.overlay;

  # WIP:
  # https://ocaml.org/manual/5.4/flambda.html
  main-optimized = self.main.overrideScope (
    final: prev: {
      nixpkgs = prev.nixpkgs.extend (
        final: prev: { ocaml = prev.ocaml.override { flambdaSupport = true; }; }
      );
      dune = prev.dune.override { inherit (final.nixpkgs) ocaml; };
      inherit (final.nixpkgs) ocaml;
    }
  );
})
