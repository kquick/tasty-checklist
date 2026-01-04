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
      # Lock to revision 6cd1f32 because the subsequent changes replace lens with
      # microlens-pro, and the latter was only introduced in Feb 2024 (circa GHC
      # 9.8.3), so it's not available for older nixpkgs configurations.  As of
      # 2025 Dec, there are no functional changes in parameterized-utils, just
      # dependency changes, and there is intent to remove the microlens-pro
      # dependency from parameterized-utils, so freeze this until that occurs.
      url = "github:GaloisInc/parameterized-utils/6cd1f32";
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
