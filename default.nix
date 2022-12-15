let pkgs = import ./nixpkgs.nix;
in pkgs.haskellPackages.callCabal2nix "unleash-client-haskell-core" ./. { }
