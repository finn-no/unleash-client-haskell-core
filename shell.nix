let pkgs = import ./nixpkgs.nix;
in pkgs.mkShell {
  buildInputs = with pkgs; [ cabal-install haskell-language-server ];
  inputsFrom = [ (import ./default.nix).env ];
}
