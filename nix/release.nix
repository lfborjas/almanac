let
  pkgs = import ./packages.nix {};
in
  { 
    almanac = pkgs.haskellPackages.almanac;
    dist = pkgs.haskellPackages.almanac-dist;    
    doc = pkgs.haskellPackages.almanac-docs;    
  }
