
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module Almanac.Event.Crossing where

import Almanac.Import ( concatForEach )
import SwissEphemeris
    ( Planet,
      PlanetMotion(DirectMotion, RetrogradeMotion),
      JulianDayTT )
import Data.Sequence (Seq(..))
import SwissEphemeris.Precalculated
    ( Ephemeris(ephePositions, epheDate),
      EphemerisPosition(epheLongitude, ephePlanet) )
import qualified Data.Map as M
import Data.Foldable (Foldable(toList))
import Data.Maybe (mapMaybe)
import Almanac.Event.Types
    ( House,
      Zodiac(..),
      Crossing(..),
      Event(HouseIngress, ZodiacIngress),
      singleton,
      Grouped,
      Aggregate(Aggregate), IsEclipticBand(..) )

getCrossings' :: IsEclipticBand a => (Crossing a -> Event ) -> [Planet] -> [a] -> Seq (Ephemeris Double) -> Grouped Planet Event
getCrossings' mkEvent selectedPlanets degreesToCross (pos1 :<| pos2 :<| _) =
  concatForEach (zip (toList $ ephePositions pos1) (toList $ ephePositions pos2)) $ \(p1, p2) ->
    if ephePlanet p1 `notElem` selectedPlanets then
      mempty
    else
      Aggregate
        $ M.fromList
        $ map (\c -> (ephePlanet p1, singleton $ mkEvent c))
        $ mapMaybe (mkCrossing (epheDate pos1, p1) (epheDate pos2, p2))
        degreesToCross

getCrossings' _ _ _ _ = mempty

getZodiacCrossings :: [Planet]-> [Zodiac] -> Seq (Ephemeris Double) -> Grouped Planet Event
getZodiacCrossings = getCrossings' ZodiacIngress

getHouseCrossings :: [Planet] -> [House] -> Seq (Ephemeris Double) -> Grouped Planet Event
getHouseCrossings = getCrossings' HouseIngress 

mkCrossing :: IsEclipticBand a => (JulianDayTT, EphemerisPosition Double) -> (JulianDayTT, EphemerisPosition Double) -> a -> Maybe (Crossing a)
mkCrossing (d1, pos1) (d2, pos2) toCross
  | crossesDirect (epheLongitude pos1) (epheLongitude pos2) (eclipticStart toCross) =
     Just $ Crossing {
        crossingStarts = d1,
        crossingEnds = d2,
        crossingCrosses = toCross,
        crossingPlanet = ephePlanet pos1,
        crossingDirection = DirectMotion
      }
  | crossesRetrograde (epheLongitude pos1) (epheLongitude  pos2) (eclipticEnd toCross) =
      Just $ Crossing {
        crossingStarts = d1,
        crossingEnds = d2,
        crossingCrosses = toCross,
        crossingPlanet = ephePlanet pos1,
        crossingDirection = RetrogradeMotion 
      }
  | otherwise = Nothing

crossesDirect :: Double -> Double -> Double -> Bool
crossesDirect p1 p2 toCross =
  if abs (p1 - p2) >= 100 && toCross == 0 then
    p1 <= (toCross + 360) && p2 >= toCross
  else
    p1 <= toCross && p2 > toCross

crossesRetrograde :: Double -> Double -> Double -> Bool
crossesRetrograde p1 p2 toCross =
  if abs (p1 - p2) >= 100 then
    p1 >= toCross && p2 < (toCross + 360)
  else
    p1 >= toCross && p2 < toCross
