{
  lib,
}:

lib.makeExtensible (self: {
  pname = "0wm-server";

  overlay = final: prev: {
    # You can add overrides here
    ${self.pname} = prev.${self.pname}.overrideAttrs (_: {
      # Prevent the ocaml dependencies from leaking into dependent environments
      doNixSupport = false;

      postBuild = ''
        dune build src/zwm.exe
      '';

      postInstall = ''
        mkdir -p $out/bin
        cp -r _build/default/src/zwm.exe $out/bin/0wm-server
      '';
    });
  };
})
