{
  inputs = {
    opam-nix.url = "github:tweag/opam-nix";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.follows = "opam-nix/nixpkgs";
  };
  outputs =
    {
      self,
      flake-utils,
      opam-nix,
      nixpkgs,
    }:
    let
      package = "zenon";
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        on = opam-nix.lib.${system};
        scope = on.buildOpamProject { } package ./. { ocaml-base-compiler = "*"; };
        overlay = final: prev: {
          # Your overrides go here
        };

        packages = scope.overrideScope overlay;
      in
      {
        legacyPackages = packages;

        packages.default = packages.${package};
      }
    );
}
