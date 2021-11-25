{ mkDerivation, base, directory, fetchgit, hpack, hspec
, hspec-discover, lib, QuickCheck, random, time, vector
}:
mkDerivation {
  pname = "swiss-ephemeris";
  version = "1.4.1.0";
  src = fetchgit {
    url = "https://github.com/lfborjas/swiss-ephemeris";
    sha256 = "0wx53bkn8x4vmllalhmkzh20rj8yfn3vp58jw9x2xp5k4wy93kh1";
    rev = "18ab38fa3646942afdea48a82e180e7a593f2643";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base time vector ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base directory hspec QuickCheck random time vector
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/lfborjas/swiss-ephemeris#readme";
  description = "Haskell bindings for the Swiss Ephemeris C library";
  license = lib.licenses.agpl3;
}
