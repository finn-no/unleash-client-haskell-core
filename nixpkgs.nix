let
  url = "https://github.com/nixos/nixpkgs/archive/${
      builtins.readFile ./nixpkgs.hash
    }.tar.gz";
in import (fetchTarball url) { }
