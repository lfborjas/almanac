
module Almanac.EclipticLongitude (
  EclipticLongitude(..),
  (<->), shortestDistance,
  (/-/), semicircleDistance,
  toEclipticLongitude
) where

import SwissEphemeris ( HasEclipticLongitude(..) )
import Data.Fixed (mod')

newtype EclipticLongitude = 
  EclipticLongitude Double
  deriving (Eq, Show)
  
instance HasEclipticLongitude EclipticLongitude where
  getEclipticLongitude (EclipticLongitude l) = l
  
instance Num EclipticLongitude where
  (EclipticLongitude el1) + (EclipticLongitude el2) =
    clampLng $ el1 + el2  
  (EclipticLongitude el1) * (EclipticLongitude el2) =
    clampLng $ el1 * el2  
  (EclipticLongitude el1) - (EclipticLongitude el2) =
    clampLng $ el1 - el2
  abs = EclipticLongitude . abs . getEclipticLongitude
  signum = EclipticLongitude . signum . getEclipticLongitude
  fromInteger = EclipticLongitude . fromInteger 

clampLng :: Double -> EclipticLongitude
clampLng n =
  EclipticLongitude $ rectified `mod'` 360
  where
    rectified = if n < 0 then n + 360 else n

-- | Return the shortest distance, in degrees, between two longitudes,
-- keeping it between 0 and 360   
shortestDistance :: EclipticLongitude -> EclipticLongitude -> Double
shortestDistance (EclipticLongitude a) (EclipticLongitude b) =
  180 - abs(abs(a - b) `mod'` 360 - 180)
  
infixl 6 <->
(<->) :: EclipticLongitude -> EclipticLongitude -> Double 
(<->) = shortestDistance

-- | Return normalized distance (not necessarily the shortest,
-- but impervious to 0/360 jumps,) between two longitudes
-- keeping it between -180 and 180. Useful when one wants
-- a distance that preserves signum.
semicircleDistance :: EclipticLongitude -> EclipticLongitude -> Double
semicircleDistance a b =
  if dif >= 180.0 then dif - 360.0 else dif
  where
    (EclipticLongitude dif) = a - b

infixl 6 /-/
(/-/) :: EclipticLongitude -> EclipticLongitude -> Double
(/-/) = semicircleDistance

{-
^ from:
https://github.com/lfborjas/swiss-ephemeris/blob/7edfb51ed12b301ffb44811c472f864a49cc7f16/csrc/swephlib.c#L3824-L3829
double CALL_CONV swe_difdeg2n(double p1, double p2)
{ double dif;
  dif = swe_degnorm(p1 - p2);
  if (dif  >= 180.0) return (dif - 360.0);
  return (dif);
}-}

toEclipticLongitude :: HasEclipticLongitude a => a -> EclipticLongitude
toEclipticLongitude = EclipticLongitude . getEclipticLongitude
