let
    project = import ./default.nix;
in
project.shellFor {
    withHoogle = true;
    tools = {
        cabal = "3.2.0.0";
        hlint = "latest";
        haskell-language-server = "latest";
    };

    buildInputs = [(import <nixpkgs> {}).git];
    exactDeps = true;
}