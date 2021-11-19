
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Almanac.Ephemeris (
  EventExactDates,
  worldAlmanac,
  natalAlmanac,
  eventExactAt,
  eventStartsAt,
  eventEndsAt,
  eventsWithExactitude,
  filterEvents,
  indexByDay,
  indexedByDay
) where

import Almanac.Event.Types
import Almanac.Event.Transit
    ( getTransits,
      getNatalTransits,
      getCuspTransits,
      uniquePairs,
      defaultPlanets,
      allPairs,
      filteredPairs,
      selectLunarTransits,
      selectLunarCuspTransits, slowPlanets )
import Almanac.Event.Crossing
    ( getZodiacCrossings, getHouseCrossings, westernZodiacSigns )
import Almanac.Event.PlanetStation ( getRetrogrades )
import Almanac.Event.LunarPhase ( mapLunarPhases )
import Almanac.Event.Eclipse ( allEclipses, getEclipseDate )

import qualified Control.Foldl as L
import qualified Streaming.Prelude as S
import Streaming ( Of((:>)), lift, MonadIO(liftIO) )
import SwissEphemeris
import Control.Category ((>>>))
import qualified Data.Sequence as Sq
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Functor ((<&>))
import Data.Foldable (foldMap', toList)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Time
import SwissEphemeris.Precalculated
import Data.Function
import Data.Either (partitionEithers)
import Data.List (intersperse, nub)

-------------------------------------------------------------------------------
-- FUNCTIONS THAT AGGREGATE EVENTS
-------------------------------------------------------------------------------

worldAlmanac :: UTCTime -> UTCTime -> IO (Sq.Seq Event)
worldAlmanac start end = do
  Just ttStart <- toJulianDay start
  Just ttEnd   <- toJulianDay end
  -- TODO: expose the function that can produce both at the same time
  Just utStart <- toJulianDay start
  Just utEnd   <- toJulianDay end
  let ephe = streamEpheJDF ttStart ttEnd
  (retro, cross, slowTransits, lun) :> _ <-
    ephe
    & ephemerisWindows 2
    & L.purely S.fold mkAlmanac

  ecl <- allEclipses utStart utEnd
  let eclSq = Sq.fromList $ map Eclipse ecl

  pure $ retro <> cross <> slowTransits <> lun <> eclSq
  where
    mkAlmanac =
      (,,,) <$> L.foldMap getRetrogrades collapse
            <*> L.foldMap (getZodiacCrossings (tail defaultPlanets) westernZodiacSigns) collapse
            <*> L.foldMap (getTransits slowPairs) collapse
            <*> L.foldMap mapLunarPhases getMerged

slowPairs :: [(Planet, Planet)]
slowPairs =
  filteredPairs
    uniquePairs
    slowPlanets
    defaultPlanets


collapse :: Aggregate grouping (MergeSeq Event) -> Sq.Seq Event
collapse = getAggregate >>> F.fold >>> getMerged

natalAlmanac :: GeographicPosition -> UTCTime -> UTCTime -> UTCTime -> IO (Sq.Seq Event)
natalAlmanac geo birth start end = do
  Just startTT <- toJulianDay start
  Just endTT   <- toJulianDay end
  Just birthTT  <- toJulianDay birth
  Just birthUT1 <- toJulianDay birth

  let ephe = streamEpheJDF startTT endTT
  natalEphe' <- readEphemerisEasy True birthTT
  case natalEphe' of
    Left e -> fail e
    Right natalEphe -> do
      CuspsCalculation{houseCusps} <- calculateCusps Placidus birthUT1 geo
      let houses = zipWith House [I .. XII] houseCusps
      (cross, trns, cuspTrns) :> _ <-
        ephe
        & ephemerisWindows 2
        & L.purely S.fold (mkAlmanac natalEphe houses)

      lun <- selectLunarTransits startTT endTT natalEphe
      lunCusps <- selectLunarCuspTransits startTT endTT (filterHouses houses)

      pure $ cross <> trns <> cuspTrns <> collapse lun <> collapse lunCusps
  where
    -- all planets are considered, except a transiting Moon
    defaultNatalPairings = filteredPairs allPairs (tail defaultPlanets) defaultPlanets
    mkAlmanac n houses =
      (,,) <$> L.foldMap (getHouseCrossings defaultPlanets houses) collapse
           <*> L.foldMap (getNatalTransits n defaultNatalPairings) collapse
           <*> L.foldMap (getCuspTransits  (filterHouses houses) sansMoon) collapse
    filterHouses houses =
      houses & filter (houseName >>> (`elem` [I, X]))
    sansMoon = filter (Moon /=) defaultPlanets

-------------------------------------------------------------------------------
-- INDEXING UTILITIES 
-------------------------------------------------------------------------------

type EventExactDates = (Event, [UTCTime])

-- | Given a sequence of events, compute their moments of exactitude
eventsWithExactitude :: Sq.Seq Event -> IO (Sq.Seq EventExactDates)
eventsWithExactitude =
  mapM addExactitude
  where
    addExactitude evt = do
      exacts <- eventExactAt evt
      pure (evt, exacts)

-- | Somewhat opinionated event filter:
-- If the event is a transit, only keep it if the transiting body is a slow planet,
-- or the final orb in the interval is very close, or the event has exactitude
-- moments happening.
filterEvents :: Sq.Seq EventExactDates -> Sq.Seq EventExactDates
filterEvents = Sq.filter isRelevantEvent

-- | Arbitrary test to see if an event is relevant enough to show in
-- a UI, or process further.
isRelevantEvent :: EventExactDates -> Bool
isRelevantEvent evt@(PlanetaryTransit t, _e) =
  isSlowTransit t || isCloseOrb t || hasExactitude evt
isRelevantEvent evt@(HouseTransit t, _e) =
  isSlowTransit t || isCloseOrb t || hasExactitude evt
isRelevantEvent e = hasExactitude e

-- | Determine if the last orb for a planet in a given
-- interval is close to exactitude; works best for 24 hour
-- intervals.
isCloseOrb :: Transit a -> Bool
isCloseOrb Transit{transitOrb} = transitOrb < 3

isSlowTransit :: Transit a -> Bool
isSlowTransit Transit{transiting} =
  transiting `elem` slowPlanets

hasExactitude :: EventExactDates -> Bool
hasExactitude = not . null . snd

indexByDay :: TimeZone -> Sq.Seq EventExactDates -> M.Map Day (Sq.Seq EventExactDates)
indexByDay tz events =
  foldMap' repeatPerEvent events
    & map (first getDay)
    & M.fromListWith (<>)
  where
    repeatPerEvent e@(_evt, exacts) =
      zip (map (utcToZonedTime tz) exacts) (repeat $ Sq.singleton e)
    getDay (ZonedTime (LocalTime d _tod) _tz) = d

indexedByDay :: TimeZone -> Sq.Seq Event -> IO (M.Map Day (Sq.Seq EventExactDates))
indexedByDay tz evts = eventsWithExactitude evts <&> (filterEvents >>> indexByDay tz)
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

-------------------------------------------------------------------------------
-- STREAMING UTILITIES
-------------------------------------------------------------------------------

streamEpheJD :: MonadIO m => (String -> m x)
  -> JulianDayTT
  -> JulianDayTT
  -> S.Stream (S.Of (Ephemeris Double)) m ()
streamEpheJD onError start end =
  S.each [start .. end]
  & S.mapM (liftIO . readEphemerisEasy False)
  & S.partitionEithers
  -- thanks, ocharles:
  -- https://www.reddit.com/r/haskell/comments/5x2g0r/streaming_package_vs_pipes_conduit_question_on/def39od?utm_source=share&utm_medium=web2x&context=3
  & S.mapM_ (lift . onError)

streamEpheJDF :: (MonadIO m, MonadFail m) => JulianDayTT -> JulianDayTT -> S.Stream (S.Of (Ephemeris Double)) m ()
streamEpheJDF = streamEpheJD fail

-- | Given a stream of ephemeris, produce "windowed"
-- steps
ephemerisWindows :: Monad m =>
  Int
  -> S.Stream (S.Of (Ephemeris Double)) m b
  -> S.Stream (S.Of (Sq.Seq (Ephemeris Double))) m b
ephemerisWindows = S.slidingWindow