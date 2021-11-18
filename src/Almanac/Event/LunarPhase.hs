
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module Almanac.Event.LunarPhase where
import SwissEphemeris.Precalculated
import SwissEphemeris
import Almanac.EclipticLongitude
import Data.Sequence (Seq ((:<|)))
import Almanac.Event.Types
import Control.Applicative (liftA2)

-- | Just to make it play nice with the other folds that look at two days
-- at once.
mapLunarPhases :: Seq (Ephemeris Double) -> MergeSeq Event
mapLunarPhases (day1 :<| day2 :<| _) =
  let sun1  = day1 `forPlanet` Sun
      sun2  = day2 `forPlanet` Sun
      moon1 = day1 `forPlanet` Moon
      moon2 = day2 `forPlanet` Moon
      sunPos = liftA2 (,) sun1 sun2
      moonPos = liftA2 (,) moon1 moon2
      phaseInfo = mkLunarPhase <$> sunPos <*> moonPos
      build (p, _changed) = LunarPhaseInfo p (epheDate day1) (epheDate day2)
  in maybe mempty (singleton . LunarPhase . build) phaseInfo
mapLunarPhases  _ = mempty

-- | Get the phase the Moon is currently in
mkLunarPhase :: (EphemerisPosition Double, EphemerisPosition Double) -> (EphemerisPosition Double, EphemerisPosition Double) -> (LunarPhaseName, Bool)
mkLunarPhase (sun1, sun2) (moon1, moon2) = 
  (phaseName', phaseChanged)
  where
    phaseName' =
      if | phase == 0   -> NewMoon
         | phase == 45  -> WaxingCrescent
         | phase == 90  -> FirstQuarter
         | phase == 135 -> WaxingGibbous 
         | phase == 180 -> FullMoon
         | phase == 225 -> WaningGibbous 
         | phase == 270 -> LastQuarter 
         | otherwise    -> WaningCrescent 
    phaseMod = 45
    oldPhase = floor $ diff1 / phaseMod :: Integer
    newPhase = floor $ diff2 / phaseMod :: Integer
    diff1 = getEclipticLongitude $ toEclipticLongitude moon1 - toEclipticLongitude sun1
    diff2 = getEclipticLongitude $ toEclipticLongitude moon2 - toEclipticLongitude sun2
    phaseChanged = newPhase /= oldPhase
    phase = fromIntegral newPhase * phaseMod
