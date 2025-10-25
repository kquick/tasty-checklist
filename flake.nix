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
    nixpkgs2411.url = github:nixos/nixpkgs/24.11;  # GHC8.10--GHC9.12
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hedgehog-classes-src = {
      url = github:hedgehogqa/haskell-hedgehog-classes;
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

  outputs = { self, nixpkgs, nixpkgs2411, levers
            , hedgehog-classes-src
            , parameterized-utils-src
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
                  shells = pkgs.lib.genAttrs names (oneshell s);
              in shells
                 // { default = devShells.${s}.tasty-checklist_test.default; }
            ) ;

      packages = levers.eachSystem (system:
        let nixpkgs_list = [
              nixpkgs2411
              nixpkgs
            ];
            mkHaskell = name: src: overDrvOrArgs:
              levers.mkHaskellPkgs {
                inherit system;
              } nixpkgs_list name src overDrvOrArgs;
            pkgs = import nixpkgs { inherit system; };
            haskellAdj = drv:
              with (pkgs.haskell).lib;
              dontHaddock (
                dontCheck (
                  dontBenchmark (
                    # disableLibraryProfiling (
                    #   disableExecutableProfiling
                        drv)
                    # )
                )
              );
            wrap = levers.pkg_wrapper system pkgs;
        in rec {
          default = tasty-checklist;
          TESTS = wrap "tasty-checklist-TESTS" [
            tasty-checklist_test         # current version
            tasty-checklist_test.ghc810  # minimum version
            tasty-checklist_test.ghc912  # maximum version
          ];
          DOC = wrap "tasty-checklist-DOC" [
            tasty-checklist_doc         # current version
            tasty-checklist_doc.ghc810  # minimum version
            tasty-checklist_doc.ghc912  # maximum version
          ];
          tasty-checklist = mkHaskell "tasty-checklist" self {
            inherit parameterized-utils;
            adjustDrv = args: haskellAdj;
          };
          tasty-checklist_test = mkHaskell "tasty-checklist" self {
            inherit parameterized-utils;
            adjustDrv = args: drv:
              with pkgs.haskell.lib; doBenchmark (doCheck (haskellAdj drv));
          };
          tasty-checklist_doc = mkHaskell "tasty-checklist" self {
            inherit parameterized-utils;
            adjustDrv = args: drv:
              with pkgs.haskell.lib.doHaddock; (haskellAdj drv);
          };
          hedgehog-classes = mkHaskell "hedgehog-classes" hedgehog-classes-src {};
          parameterized-utils = mkHaskell "parameterized-utils"
            parameterized-utils-src {
              inherit
                tasty-hedgehog
                hedgehog-classes;
            };
          tasty-hedgehog = mkHaskell "tasty-hedgehog" tasty-hedgehog-src {};
        });

      checks = levers.eachSystem (system:
        self.packages."${system}".TESTS);
    };
}
