let
  pkgs = import ./packages.nix {};
in
  { hcat = pkgs.haskellPackages.hcat; }
