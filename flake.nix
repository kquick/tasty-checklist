{
  # $ nix develop
  # $ nix build
  # $ nix develop .#tasty-checklist.ghc8104.default.env
  # $ nix build .#tasty-checklist
  # $ nix build .#tasty-checklist.ghc884

  description = "Checklist library for Haskell Tasty testing framework";

  nixConfig.bash-prompt-suffix = "tasty-checklist.env} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/22.11;
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hedgehog-src = {
      url = github:hedgehogqa/haskell-hedgehog?dir=hedgehog;
      flake = false;
    };
    hedgehog-classes-src = {
      url = github:hedgehogqa/haskell-hedgehog-classes;
      flake = false;
    };
    tasty-expected-failure-src = {
      url = github:nomeata/tasty-expected-failure;
      flake = false;
    };
    parameterized-utils-src = {
      url = github:GaloisInc/parameterized-utils;
      flake = false;
    };
    tasty-hedgehog-src = {
      url = github:qfpl/tasty-hedgehog;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, levers
            , hedgehog-src
            , hedgehog-classes-src
            , parameterized-utils-src
            , tasty-expected-failure-src
            , tasty-hedgehog-src
            }:
     let shellWith = pkgs: adds: drv: drv.overrideAttrs(old:
           { buildInputs = old.buildInputs ++ adds pkgs; });
         shellPkgs = pkgs: [ pkgs.cabal-install
                           ];
     in
    rec
    {
      defaultPackage = levers.eachSystem (s:
        self.packages.${s}.tasty-checklist.default);

      devShell = levers.eachSystem (s:
        let pkgs = import nixpkgs { system=s; };
            in shellWith pkgs shellPkgs defaultPackage.${s}.env);

      devShells =
        let oneshell = s: n:
              let pkgs = import nixpkgs { system=s; };
              in levers.variedTargets
                { ghcver = levers.validGHCVersions pkgs.haskell.compiler; }
                ( { ghcver, ... } @ vargs:
                  shellWith pkgs shellPkgs
                    (self.packages.${s}.${n}.${ghcver}.env)
                );
        in
          levers.eachSystem
            (s:
              let pkgs = import nixpkgs { system=s; };
                  names = builtins.attrNames (self.packages.${s});
              in pkgs.lib.genAttrs names (oneshell s)
            ) ;

      packages = levers.eachSystem (system:
        let mkHaskell = levers.mkHaskellPkg {
              inherit nixpkgs system;
            };
            pkgs = import nixpkgs { inherit system; };
        in rec {
          tasty-checklist = mkHaskell "tasty-checklist" self {
            inherit parameterized-utils tasty-expected-failure;
          };
          tasty-expected-failure = mkHaskell "tasty-expected-failure"
            tasty-expected-failure-src {
              inherit tasty-hedgehog;
            };
          hedgehog = mkHaskell "hedgehog" "${hedgehog-src}/hedgehog" {};
          hedgehog-classes = mkHaskell "hedgehog-classes" hedgehog-classes-src {
            inherit hedgehog;
          };
          parameterized-utils = mkHaskell "parameterized-utils"
            parameterized-utils-src {
              inherit tasty-hedgehog hedgehog-classes;
            };
          tasty-hedgehog = mkHaskell "tasty-hedgehog" tasty-hedgehog-src {
            inherit hedgehog;
          };
        });

      checks = levers.eachSystem (system:
        let mkHaskell = levers.mkHaskellPkg {
              inherit nixpkgs system;
            };
            pkgs = import nixpkgs { inherit system; };
        in {
          tasty-checklist-check = mkHaskell "tasty-checklist-check" self {
            adjustDrv = args: drv: pkgs.haskell.lib.doCheck drv;
            parameterized-utils = self.packages.${system}.parameterized-utils;
            tasty-expected-failure = self.packages.${system}.tasty-expected-failure;
          };
        });

    };
}
