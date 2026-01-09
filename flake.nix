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
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        on = opam-nix.lib.${system};

        # Define the package name
        packageName = "zenon";

        # Build the project scope
        scope = on.buildOpamProject { } packageName ./. {
          ocaml-base-compiler = "*";
        };

        mainPackage = scope.${packageName};
      in
      {
        packages = {
          default = mainPackage;
          ${packageName} = mainPackage;
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ mainPackage ];
          buildInputs = with pkgs; [
            ocaml
            opam
            ocamlformat
            ocamlPackages.ocaml-lsp
          ];
        };

        legacyPackages = scope;
      }
    );
}
