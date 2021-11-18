{system ? builtins.currentSystem}:
# NOTE(luis)
# All packages listed here will be built from source, as they're not
# 'blessed' in our pinned nix version. The sources themselves _are_
# obtained from cache.nixos.org, but need to be rebuilt regardless.
# Exercise restraint!
let
  dontCheck   = (import ./packages.nix{inherit system;}).haskell.lib.dontCheck;
  doJailbreak = (import ./packages.nix{inherit system;}).haskell.lib.doJailbreak;
in (super: {
  # don't run tests for these
  # they currently don't build in nix due to bad quickcheck versions
  swiss-ephemeris = super.callPackage ./extra-pkgs/swiss-ephemeris.nix {};
  timezone-detect = dontCheck (super.callPackage ./extra-pkgs/timezone-detect.nix {}) ;
})
