let
    sources = import ./nix/sources.nix {};
    haskellNix = import sources.haskellNix {};
    pkgs = import 
        haskellNix.sources.nixpkgs-2105
        haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "haskell-starter-kit";
        src = ./.;
    };
    compiler-nix-name = "ghc8107";
}