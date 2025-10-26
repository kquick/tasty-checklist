{
  # $ nix develop
  # $ nix build
  # $ nix develop .#tasty-checklist.ghc8104.default.env
  # $ nix build .#tasty-checklist
  # $ nix build .#tasty-checklist.ghc884

  description = "Checklist library for Haskell Tasty testing framework";

  nixConfig.bash-prompt-suffix = "tasty-checklist.env} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    parameterized-utils-src = {
      url = github:GaloisInc/parameterized-utils;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, levers
            , parameterized-utils-src
            }:
    rec
    {
      devShells = levers.haskellShells
        { inherit nixpkgs;
          flake = self;
          defaultPkg = "tasty-checklist";
          # additionalPackages = pkgs.haskell.packages.ghc8107.profiteur
        };

      packages = levers.eachSystem (system:
        let mkHaskell = levers.mkHaskellPkg {inherit nixpkgs system;};
            pkgs = import nixpkgs { inherit system; };
        in rec {
          default = tasty-checklist;
          tasty-checklist = mkHaskell "tasty-checklist" self {
            inherit parameterized-utils;
          };
          parameterized-utils = mkHaskell "parameterized-utils"
            parameterized-utils-src {
            };
        });
    };
}
