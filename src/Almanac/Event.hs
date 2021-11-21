{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Almanac.Event (
  -- * Event transformation
  eventsWithExactitude,
  -- * Temporal properties
  eventExactAt,
  eventStartsAt,
  eventEndsAt
) where

import Almanac.Event.Eclipse (getEclipseDate)
import Almanac.Event.Types
import Data.Either (partitionEithers)
import Data.Foldable (foldMap', toList)
import Data.Function
import Data.List (intersperse, nub)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import qualified Data.Sequence as Sq
import Data.Time
import SwissEphemeris

-- | Given a sequence of events, compute their moments of exactitude
eventsWithExactitude :: Sq.Seq Event -> IO (Sq.Seq ExactEvent)
eventsWithExactitude =
  mapM addExactitude
  where
    addExactitude evt = do
      exacts <- eventExactAt evt
      pure $ ExactEvent evt exacts

-------------------------------------------------------------------------------
-- TEMPORAL UTILITIES
-------------------------------------------------------------------------------

-- | Get all moments of exactitude in the span of an @Event@; in reality, 
-- only Transits are liable to have more than one moment of exactitude (if they
-- span long enough -- notice that we don't handle the edge case of a transit
-- being exact twice in the same day: we just produce one of the crossings for
-- that day.) In a future update, we'll be able to correctly indicate
-- said multiple crossings (e.g. if a planet happens to just change
-- direction in a given interval and do a double-crossing.)
eventExactAt :: Event -> IO [UTCTime]
eventExactAt (DirectionChange PlanetStation{stationStarts, stationEnds, stationPlanet}) = do
  changesAt <- directionChangeBetween stationPlanet stationStarts stationEnds
  case changesAt of
    Left _e -> pure []
    Right (dirChangesAt, _) -> mapM fromJulianDay [dirChangesAt]
eventExactAt (LunarPhase LunarPhaseInfo{lunarPhaseName, lunarPhaseStarts}) =
    moonPhaseExactAt lunarPhaseName lunarPhaseStarts (succ lunarPhaseStarts)
    >>= crossingAsList
eventExactAt (Eclipse ecl) =
  mapM fromJulianDay [getEclipseDate ecl]
eventExactAt (PlanetaryTransit t) = transitExactAt t
eventExactAt (HouseTransit t) = transitExactAt t
eventExactAt (ZodiacIngress xn)= crossingExactAt xn
eventExactAt (HouseIngress xn) = crossingExactAt xn

crossingExactAt :: HasEclipticLongitude a => Crossing a -> IO [UTCTime]
crossingExactAt Crossing{crossingPlanet, crossingCrosses, crossingStarts, crossingEnds}=
  allCrossingsBetween crossingPlanet (getEclipticLongitude crossingCrosses) crossingStarts crossingEnds
  >>= crossingsAsList

transitExactAt :: Transit a -> IO [UTCTime]
transitExactAt Transit{transitPhases, transitCrosses, transiting, transitIsExact} =
  if not . null $ transitIsExact then
    -- the Moon (and other non-retrograde bodies, like the Sun)
    -- may already have had its exactitude moments calculated.
    mapM fromJulianDay transitIsExact
  else
    transitPhases
      & toList
      & filter ((`elem` [TriggeredDirect, TriggeredRetrograde]) . phaseName)
      & foldMap' triggeredAt
  where
    triggeredAt TransitPhase{phaseStarts, phaseEnds} = do
      allCrossingsBetween transiting (getEclipticLongitude transitCrosses) phaseStarts phaseEnds
      >>= crossingsAsList

crossingAsList :: Either String JulianDayTT -> IO [UTCTime]
crossingAsList crossesAt = do
  case crossesAt of
    Left _e -> pure []
    Right crossesAtTT -> mapM fromJulianDay [crossesAtTT]

crossingsAsList :: Either String (NonEmpty JulianDayTT) -> IO [UTCTime]
crossingsAsList crossesAt = do
  case crossesAt of
    Left _e -> pure []
    Right crossingsAtTT -> mapM fromJulianDay $ toList crossingsAtTT


-- Get the moment an event starts at, in UTC
eventStartsAt :: Event -> IO UTCTime
eventStartsAt (DirectionChange evt) = fromJulianDay $ stationStarts evt
eventStartsAt (LunarPhase evt) = fromJulianDay $ lunarPhaseStarts evt
eventStartsAt (Eclipse evt) = fromJulianDay $ getEclipseDate evt
eventStartsAt (PlanetaryTransit evt) = fromJulianDay $ transitStarts evt
eventStartsAt (HouseTransit evt) = fromJulianDay $ transitStarts evt
eventStartsAt (ZodiacIngress evt) = fromJulianDay $ crossingStarts evt
eventStartsAt (HouseIngress evt) = fromJulianDay $ crossingEnds evt

-- Get the moment an event ends at, in UTC
eventEndsAt :: Event -> IO UTCTime
eventEndsAt (DirectionChange evt) = fromJulianDay $ stationEnds evt
eventEndsAt (LunarPhase evt) = fromJulianDay $ lunarPhaseEnds evt
eventEndsAt (Eclipse evt) = fromJulianDay $ getEclipseDate evt
eventEndsAt (PlanetaryTransit evt) = fromJulianDay $ transitEnds evt
eventEndsAt (HouseTransit evt) = fromJulianDay $ transitEnds evt
eventEndsAt (ZodiacIngress evt) = fromJulianDay $ crossingEnds evt
eventEndsAt (HouseIngress evt) = fromJulianDay $ crossingEnds evt

-- | Given a @Planet@, a longitude it crosses, and a start and end
-- @JulianDay@s that may contain /one/ change of direction, 
-- find the one or two moments the planet crosses the given longitude,
-- from a geocentric perspective (retrogrades are taken into account.)
-- 
-- _NOTE_: works best when the interval given is both short and known
-- to contain at least one crossing. Unlike 'crossingBetween', this 
-- function sacrifices a bit of performance (has to determine if a change
-- of direction happens) for accuracy: if the planet changes direction within
-- the interval, it /may/ cross over the given longitude a second time,
-- hence the @NonEmpty@ success case, vs. a single @JulianDay@.
allCrossingsBetween
  :: SingTSI ts
  => Planet
  -> Double
  -> JulianDay ts
  -> JulianDay ts
  -> IO (Either String (NonEmpty (JulianDay ts)))
allCrossingsBetween planet toCross start end = do
  dirChange <- directionChangeBetween planet start end
  case dirChange of
    Left _noChange -> do
      onlyCrossing <- crossingBetween planet toCross start end
      -- NOTE(luis) in >= base-4.15, we'll get:
      -- `pure $ singleton <$> onlyCrossing`
      pure $ fmap (:| []) onlyCrossing
    Right (changesAt, _dir) -> do
      firstCrossing  <- crossingBetween planet toCross start changesAt
      secondCrossing <- crossingBetween planet toCross changesAt end
      let (errors, crossings) = partitionEithers [firstCrossing, secondCrossing]
      if null crossings then
        pure . Left . mconcat $ intersperse ", " errors
      else
        pure . Right . fromList . nub $ crossings
