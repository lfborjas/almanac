{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Almanac.Query (
  -- * Reference types
  QueryType(..),
  ReferenceEvent(..),
  QueryArgs(..),
  -- * Opaque types
  Query,
  Interval,
  -- * Smart constructors
  query,
  -- * Query execution
  runQuery
) where

import Data.Time (UTCTime)
import Data.Maybe
import Data.List.NonEmpty ( NonEmpty, nub, partition, toList )
import SwissEphemeris
import Prelude hiding (filter)
import Data.Sequence (Seq, fromList)
import Almanac.Event.Types
import Data.Functor.Of (Of((:>)))
import qualified Streaming.Prelude as S
import qualified Control.Foldl as L
import Data.Function ( (&) )
import SwissEphemeris.Precalculated (Ephemeris, readEphemerisEasy)
import Almanac.Event.PlanetStation (getRetrogrades)
import Almanac.Event.Eclipse (allEclipses)
import Almanac.Event.Crossing
import Almanac.Event.Transit
import Almanac.Event.LunarPhase
import qualified Data.List as List
import Control.Category ((>>>))
import Streaming (lift, MonadIO (..))
import qualified Data.Foldable as F


data ReferenceEvent =
  ReferenceEvent {
    eventTime :: !UTCTime,
    eventLocation :: !GeographicPosition
  } deriving (Eq, Show)

data QueryType
  = QueryDirectionChange (NonEmpty Planet)
  | QueryZodiacIngress   (NonEmpty Planet)
  | QueryHouseIngress    (NonEmpty Planet)
  | QueryPlanetaryMundaneTransit (NonEmpty (Planet, Planet))
  | QueryPlanetaryNatalTransit (NonEmpty (Planet, Planet))
  | QueryCuspTransit  (NonEmpty (Planet, HouseName))
  | QueryLunarNatalTransit (NonEmpty Planet)
  | QueryLunarCuspTransit  (NonEmpty HouseName)
  | QueryLunarPhase
  | QueryEclipse
  deriving (Eq, Show)

data Interval =
  Interval {
    start :: !UTCTime,
    end :: !UTCTime
  } deriving (Eq, Show)

-- TODO(luis) should also check that the range is congruent with
-- the included ephemeris.
mkInterval :: UTCTime -> UTCTime -> Either String Interval
mkInterval start' end'
  | start' < end' = Right $ Interval start' end'
  | otherwise = Left "Invalid interval"

data Query
  = MundaneQuery Interval (NonEmpty QueryType)
  | NatalQuery   Interval (NonEmpty QueryType) ReferenceEvent HouseSystem

data QueryArgs
  = QueryArgs{
    queryStart :: UTCTime,
    queryEnd :: UTCTime,
    queryTypes :: NonEmpty QueryType,
    queryEventArgs :: Maybe (ReferenceEvent, HouseSystem)
  } deriving (Eq, Show)

query :: QueryArgs -> Either String Query
query QueryArgs{queryStart, queryEnd, queryTypes, queryEventArgs} =
  case queryEventArgs of
    Just (refEvent, houseSystem) ->
      NatalQuery <$> interval' <*> queries' <*> Right refEvent <*> Right houseSystem
    Nothing ->
      MundaneQuery <$> interval' <*> queries'
  where
    interval' = mkInterval queryStart queryEnd
    queries'  = mkQueries  queryTypes queryEventArgs
    mkQueries qs rf =
      if (not . any requiresReferenceEvent $ qs) && isNothing rf then
        Left "Reference Event must be provided"
      else
        Right . nub $ qs

runQuery :: Query -> IO (Seq Event)
runQuery (MundaneQuery iv qts) = runMundaneQuery iv qts
runQuery (NatalQuery   iv qts rf hs) = runNatalQuery iv qts rf hs

runMundaneQuery :: Interval -> NonEmpty QueryType -> IO (Seq Event)
runMundaneQuery i@Interval{start,end} qs = do
  Just ttStart <- toJulianDay start
  Just ttEnd   <- toJulianDay end
  let (pureQueries, impureQueries) = partition isFoldable qs
      ephe = streamEpheJDF ttStart ttEnd
  -- construct a fold of all pure queries to traverse the ephemeris
  -- only once; results in a list of sequences, one entry per query.
  pureFolded :> _ <-
   ephe
   & ephemerisWindows 2
   & L.purely S.fold (traverse (toFold Nothing) pureQueries)

  -- impure queries have to be executed in sequence, though they
  -- don't have to traverse the whole ephemeris
  impureFolded <- traverse (execQuery i Nothing) impureQueries

  pure $ mconcat pureFolded <> mconcat impureFolded

runNatalQuery :: Interval -> NonEmpty QueryType -> ReferenceEvent -> HouseSystem -> IO (Seq Event)
runNatalQuery i@Interval{start,end} qs ReferenceEvent{eventTime, eventLocation} houseSystem = do
  Just ttStart <- toJulianDay start
  Just ttEnd   <- toJulianDay end
  Just ttEvent <- toJulianDay eventTime
  Just ut1Event <- toJulianDay eventTime
  referenceEphemeris <- readEphemerisEasy True ttEvent
  let (pureQueries, impureQueries) = partition isFoldable qs
      ephe = streamEpheJDF ttStart ttEnd
  case referenceEphemeris of
    Left e -> fail e
    Right ref -> do
      CuspsCalculation{houseCusps} <- calculateCusps houseSystem ut1Event eventLocation
      let houses = zipWith House [I .. XII] houseCusps
      pureFolded :> _ <-
        ephe
        & ephemerisWindows 2
        & L.purely S.fold (traverse (toFold (Just (ref, houses))) pureQueries)

      impureFolded <- traverse (execQuery i (Just (ref, houses))) impureQueries

      pure $ mconcat pureFolded <> mconcat impureFolded

-------------------------------------------------------------------------------
-- | Does a reference event need to be provided?
requiresReferenceEvent :: QueryType -> Bool
requiresReferenceEvent (QueryHouseIngress _) = True
requiresReferenceEvent (QueryCuspTransit _) = True
requiresReferenceEvent (QueryLunarNatalTransit _) = True
requiresReferenceEvent (QueryPlanetaryNatalTransit _) = True
requiresReferenceEvent _ = False

toFold :: Maybe (Ephemeris Double, [House]) -> QueryType -> L.Fold (Seq (Ephemeris Double)) (Seq Event)
toFold referenceData =
  \case
    (QueryDirectionChange _planets) ->
      L.foldMap getRetrogrades collapse
    (QueryZodiacIngress planets)  ->
      L.foldMap (getZodiacCrossings (toList planets) westernZodiacSigns) collapse
    (QueryHouseIngress planets) ->
      case referenceData of
        Just (_ephe, houses) ->
          L.foldMap (getHouseCrossings (toList planets) houses) collapse
        Nothing ->
          mempty
    (QueryPlanetaryMundaneTransit pairs) ->
      L.foldMap (getTransits (toList pairs)) collapse
    (QueryPlanetaryNatalTransit pairs) ->
      case referenceData of
        Just (ephe, _houses) ->
          L.foldMap (getNatalTransits ephe (toList pairs)) collapse
        Nothing ->
          mempty
    (QueryCuspTransit pairs) ->
      case referenceData of
        Just (_ephe, houses) ->
          let pairList = toList pairs
              chosenPlanets = map fst pairList
              chosenHouseNames  = List.nub $ map snd pairList
              chosenHouses = houses & List.filter (houseName >>> (`elem` chosenHouseNames))
          in L.foldMap (getCuspTransits chosenHouses chosenPlanets) collapse
        Nothing ->
          mempty
    QueryLunarPhase ->
      L.foldMap mapLunarPhases getMerged
    -- All other queries are meant to be executed directly, without constructing a Fold
    _ -> mempty

-- | Can this event be grouped with others to fold in one pass?
isFoldable :: QueryType -> Bool
isFoldable (QueryLunarNatalTransit _ ) = False
isFoldable (QueryLunarCuspTransit _ ) = False
isFoldable QueryEclipse = False
isFoldable _ = True

execQuery :: Interval -> Maybe (Ephemeris Double, [House]) -> QueryType -> IO (Seq Event)
execQuery Interval{start,end} referenceData =
  \case
    QueryEclipse -> do
     -- TODO: expose the function that can produce both at the same time
      Just utStart <- toJulianDay start
      Just utEnd   <- toJulianDay end
      ecl <- allEclipses utStart utEnd
      pure $ fromList $ map Eclipse ecl
    (QueryLunarNatalTransit planets) -> do
      case referenceData of
        Just (ephe, _houses) -> do
          Just ttStart <- toJulianDay start
          Just ttEnd   <- toJulianDay end
          -- TODO(luis) allow choosing planets here
          lun <- selectLunarTransits ttStart ttEnd ephe (toList planets)
          pure $ collapse lun
        Nothing -> pure mempty
    (QueryLunarCuspTransit houseNames) -> do
      case referenceData of
        Just (_ephe, houses) -> do
          Just ttStart <- toJulianDay start
          Just ttEnd   <- toJulianDay end
          let chosenHouses = houses & List.filter (houseName >>> (`elem` houseNames))
          lunCusps <- selectLunarCuspTransits ttStart ttEnd chosenHouses
          pure $ collapse lunCusps
        Nothing -> pure mempty
    -- All other queries are meant to be folded.
    _ -> pure mempty

collapse :: Aggregate grouping (MergeSeq Event) -> Seq Event
collapse = getAggregate >>> F.fold >>> getMerged

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
  -> S.Stream (S.Of (Seq (Ephemeris Double))) m b
ephemerisWindows = S.slidingWindow
