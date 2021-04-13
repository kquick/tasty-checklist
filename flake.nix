{
  # $ nix develop
  # $ nix build
  # $ nix develop .#tasty-checklist.ghc8104.default.env
  # $ nix build .#tasty-checklist
  # $ nix build .#tasty-checklist.ghc884

  description = "Checklist library for Haskell Tasty testing framework";

  nixConfig.bash-prompt-suffix = "tasty-checklist.env} ";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/20.09;
    levers = {
      type = "github";
      owner = "kquick";
      repo = "nix-levers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    tasty-expected-failure-src = {
      url = github:nomeata/tasty-expected-failure;
      flake = false;
    };
    parameterized-utils-src = {
      url = github:GaloisInc/parameterized-utils;
      flake = false;
    };
  };

  outputs = { self, nixpkgs, levers
            , parameterized-utils-src
            , tasty-expected-failure-src
            }: rec
    {
      defaultPackage = levers.eachSystem (s:
        self.packages.${s}.tasty-checklist.default);

      devShell = levers.eachSystem (s: defaultPackage.${s}.env);

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
            };
          parameterized-utils = mkHaskell "parameterized-utils"
            parameterized-utils-src {
            };
        });
    };
}
