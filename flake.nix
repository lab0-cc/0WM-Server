{
  description = "0WM Server";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.opam-repository.follows = "opam-repository";
    };
    # Used as a default repository
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      opam-nix,
      opam-repository,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        on = opam-nix.lib.${system};

        pkgs = nixpkgs.legacyPackages.${system}.extend (
          final: prev: {
            inherit (on) buildOpamProject' makeOpamRepo;
            inherit opam-repository;
          }
        );

        inherit (pkgs) lib;

        devPackagesQuery = {
          ## You can add "development" packages here.
          ## They will get added to the devShell automatically.
          # ocaml-lsp-server = "*";
          # ocamlformat = "*";
        };
        query = devPackagesQuery // {
          ## You can force versions of certain packages here, e.g:
          ## - force the ocaml compiler to be taken from opam-repository:
          # ocaml-base-compiler = "*";
          # ocaml-compiler-libs = "*";
          # ocamlbuild = "*";
          ## - or force the compiler to be taken from nixpkgs and be a certain version:
          # ocaml-system = "4.14.0";
          ## - or force ocamlfind to be a certain version:
          # ocamlfind = "1.9.2";
        };

        scopes = pkgs.callPackage ./nix/scopes.nix {
          cf = pkgs.callPackage ./nix/cf.nix { };
          dream = pkgs.callPackage ./nix/dream.nix { };
          irmin = pkgs.callPackage ./nix/irmin.nix { };
          inherit query;
          src = lib.cleanSource ./.;
          zwm-server = pkgs.callPackage ./nix/zwm-server.nix { };
        };

        # The main package containing the executable
        package = scopes.main."0wm-server";

        # Packages from devPackagesQuery
        devPackages = lib.attrValues (lib.getAttrs (lib.attrNames devPackagesQuery) scopes.main);
      in
      {
        legacyPackages = scopes.main;

        # nix build .#default -L
        packages.default = package;

        # nix develop
        devShells.default = pkgs.mkShell {
          inputsFrom = [ package ]; # add package's build dependencies
          packages =
            with scopes.main.nixpkgs;
            [
              # You can add packages from nixpkgs here
              ocamlPackages.ocamlformat
              ocamlPackages.ocaml-lsp
            ]
            ++ devPackages;
        };
      }
    );
}
