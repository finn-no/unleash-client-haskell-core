{
  description = "unleash-client-haskell-core";
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
  };
  outputs = { self, nixpkgs, flake-compat, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        unleash-client-haskell-core =
          pkgs.haskellPackages.callCabal2nix "unleash-client-haskell-core" ./.
          { };
      in {
        defaultPackage = unleash-client-haskell-core; # nix build .?submodules=1
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            cabal-install
            haskellPackages.fourmolu
            haskell-language-server
            hlint
            haskellPackages.implicit-hie
            nixfmt
            nodePackages.conventional-changelog-cli
          ];
          inputsFrom = [ unleash-client-haskell-core.env ];
        };
      });
}
