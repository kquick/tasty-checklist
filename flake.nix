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
      url = "github:GaloisInc/parameterized-utils";
      flake = false;
    };
    microlens-src = {
      url = "github:monadfix/microlens";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, levers
            , parameterized-utils-src
            , microlens-src
            }:
    rec
    {
      devShells = levers.haskellShells
        { inherit nixpkgs;
          flake = self;
          defaultPkg = "tasty-checklist";
          additionalPackages = pkgs: [ pkgs.cabal-install ];
          ghcvers = s: ["ghc912"];
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
              inherit microlens;
              adjustDrv = args: drv:
                let jailBreak_hedgehog_classes = i:
                      # Under hedgehog-classes has upper bounds that preclude
                      # GHC9.14; avoid those constraints until hedgehog-classes
                      # is updated.
                      if builtins.match "ghc91[^02]" args.ghcver != null
                         && i != null
                         && i.pname or "no-pname" == "hedgehog-classes"
                      then pkgs.haskell.lib.doJailbreak i
                      else i;
                in drv.overrideAttrs (old:
                  {
                    buildInputs = builtins.map jailBreak_hedgehog_classes
                      old.buildInputs;
                  });
          };
          microlens = mkHaskell "microlens" "${microlens-src}/microlens" { };
        });
    };
}
