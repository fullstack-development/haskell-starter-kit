{ compiler ? "ghc8104" }:

let 
    config = {
        packageOverrides = pkgs: rec {
            haskell = pkgs.haskell // {
                packages = pkgs.haskell.packages // {
                    "${compiler}" = pkgs.haskell.packages."${compiler}".override {
                        overrides = haskellPackagesNew: haskellPackagesOld: rec {

                        haskell-starter-kit =
                            pkgs.haskell.lib.dontCheck ( # Temporary solution
                                haskellPackagesNew.callPackage ./haskell-starter-kit.nix { }
                            );

                        };
                    };
                };
            };
        };
    };

    boostrap = import <nixpkgs> { };

    nixpkgs = builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json);

    src = boostrap.fetchFromGitHub {
        owner = "NixOS";
        repo = "nixpkgs";
        inherit (nixpkgs) rev sha256;
    };

    pkgs = import src { inherit config;  };
in
    {
        haskell-starter-kit = pkgs.haskell.packages.${compiler}.haskell-starter-kit;
    }
    