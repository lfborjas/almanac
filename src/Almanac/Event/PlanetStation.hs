{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module Almanac.Event.PlanetStation where

import Data.Sequence (Seq(..), ViewL(..))
import qualified Data.Map as M
import SwissEphemeris
import SwissEphemeris.Precalculated
import Data.Foldable (toList)
import Almanac.Import
import Almanac.Event.Types

-- | Given ephemeris for two consecutive days, determine if a planet has changed
-- direction, or entered a stationary phase.
getRetrogrades :: [Planet] -> Seq (Ephemeris Double) -> Grouped Planet Event
getRetrogrades chosenPlanets (pos1 :<| pos2 :<| _) =
  concatForEach (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
    case mkStation (epheDate pos1, p1) (epheDate pos2, p2) of
     Nothing -> mempty
     Just st -> 
       -- the MeanNode /appears/ direct/retrograde sometimes,
       -- but that's not astrologically significant.
       if ephePlanet p1 `elem` chosenPlanets then
         Aggregate $ M.fromList [(ephePlanet p1, singleton (DirectionChange st))]
       else
         mempty

getRetrogrades _ _ = mempty


isStationary :: ViewL PlanetStation -> Bool
isStationary EmptyL = False
isStationary (PlanetStation{stationType} :< _) =
  stationType `elem` [StationaryDirect, StationaryRetrograde]

mkStation :: (JulianDayTT, EphemerisPosition Double) -> (JulianDayTT, EphemerisPosition Double) -> Maybe PlanetStation
mkStation (d1, pos1) (d2, pos2)
  | signum (epheSpeed pos1) /= signum (epheSpeed pos2) =
    Just $ PlanetStation {
      stationStarts = d1,
      stationEnds = d2,
      stationPlanet = ephePlanet pos1,
      stationType = if epheSpeed pos1 > epheSpeed pos2 then Retrograde else Direct
     }
  | isRelativelyStationary pos1 =
    Just $ PlanetStation {
      stationStarts  = d1,
      stationEnds = d2,
      stationPlanet = ephePlanet pos1,
      stationType = if epheSpeed pos1 > epheSpeed pos2 then StationaryRetrograde else StationaryDirect
      }
  | otherwise =
  Nothing
  
isRelativelyStationary :: EphemerisPosition Double -> Bool
isRelativelyStationary EphemerisPosition{ephePlanet, epheSpeed} =
  case ephePlanet of
    Mercury -> degToSec epheSpeed <= 300
    Venus -> degToSec epheSpeed <= 180
    Mars -> degToSec epheSpeed <= 90
    Jupiter -> degToSec epheSpeed <= 60
    Saturn -> degToSec epheSpeed <= 60
    Chiron -> degToSec epheSpeed <= 20
    Uranus -> degToSec epheSpeed <= 20
    Neptune -> degToSec epheSpeed <= 10
    Pluto -> degToSec epheSpeed <= 10 
    _ -> degToSec epheSpeed <= 0

-- | Convert a degree unit to arcseconds
degToSec :: Double -> Double
degToSec = abs . (*3600)
