{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    frontend = {
      url = "path:../frontend";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, flake-utils, naersk, nixpkgs, frontend }:
    let
      perSystem = flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = (import nixpkgs) { inherit system; };

          naersk' = pkgs.callPackage naersk { };

          defaultPackage = naersk'.buildPackage {
            src = ./.;
          };
        in
        {
          # For `nix build` & `nix run`:
          defaultPackage = defaultPackage;

          devShells.default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [ rustc cargo ];
          };

          nixosModules.sdc-map-backend = import ./module.nix {
            package = defaultPackage;
            frontend = frontend;
            system = system;
          };
        }
      );
    in
    {
      nixosModules = perSystem.nixosModules;

      defaultPackage = perSystem.defaultPackage;

      # For `nix develop`:
      devShells = perSystem.devShells;
    };
}
