{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Almanac.Query (
  -- * Reference types
  MundaneQuery(..),
  NatalQuery(..),
  ReferenceEvent(..),
  Query(..),
  MundaneArgs(..),
  NatalArgs(..),
  Interval(..),
  -- * Query execution
  runQuery
) where

import Data.Time (UTCTime)
import Data.List ( nub, partition )
import Data.List.NonEmpty ( NonEmpty, toList)
import SwissEphemeris
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
import Control.Category ((>>>))
import Streaming (lift, MonadIO (..))
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE


-- | internal utility typeclass to signal which queries
-- can be composed into a one-pass fold, and which
-- require direct execution. 
class ComposableQuery a where
  canCompose :: a -> Bool

data ReferenceEvent =
  ReferenceEvent {
    eventTime :: !UTCTime,
    eventLocation :: !GeographicPosition
  } deriving (Eq, Show)

data MundaneQuery
  = QueryDirectionChange (NonEmpty Planet)
  | QueryZodiacIngress   (NonEmpty Planet)
  | QueryPlanetaryMundaneTransit (NonEmpty (Planet, Planet))
  | QueryLunarPhase
  | QueryEclipse
  deriving (Eq, Show)

instance ComposableQuery MundaneQuery where
  canCompose =
    \case
      QueryEclipse -> False
      _ -> True

data NatalQuery
  = QueryHouseIngress    (NonEmpty Planet)
  | QueryPlanetaryNatalTransit (NonEmpty (Planet, Planet))
  | QueryCuspTransit  (NonEmpty (Planet, HouseName))
  | QueryLunarNatalTransit (NonEmpty Planet)
  | QueryLunarCuspTransit  (NonEmpty HouseName)
  deriving (Eq, Show)

instance ComposableQuery NatalQuery where
  canCompose =
    \case
      QueryLunarCuspTransit _-> False
      QueryLunarNatalTransit _ -> False
      _ -> True

data Interval =
  Interval {
    start :: !UTCTime,
    end :: !UTCTime
  } deriving (Eq, Show)


data MundaneArgs =
  MundaneArgs {
    mInterval :: Interval,
    mQueries  :: NonEmpty MundaneQuery
  }

data NatalArgs =
  NatalArgs {
    nInterval :: Interval,
    nQueries  :: NonEmpty NatalQuery,
    nMundaneQueries :: [MundaneQuery],
    nReferenceEvent :: ReferenceEvent,
    nHouseSystem :: HouseSystem
  }

data Query
  = Mundane MundaneArgs
  | Natal   NatalArgs

runQuery :: Query -> IO (Seq Event)
runQuery (Mundane args) = runMundaneQuery args
runQuery (Natal   args) = runNatalQuery args

runMundaneQuery :: MundaneArgs ->IO (Seq Event)
runMundaneQuery (MundaneArgs i@Interval{start,end} qs) = do
  Just ttStart <- toJulianDay start
  Just ttEnd   <- toJulianDay end
  let (pureQueries, impureQueries) = NE.partition canCompose qs
      ephe = streamEpheJDF ttStart ttEnd
  -- construct a fold of all pure queries to traverse the ephemeris
  -- only once; results in a list of sequences, one entry per query.
  pureFolded :> _ <-
   ephe
   & ephemerisWindows 2
   & L.purely S.fold (traverse toFoldMundane pureQueries)

  -- impure queries have to be executed in sequence, though they
  -- don't have to traverse the whole ephemeris
  impureFolded <- traverse (execMundane i) impureQueries

  pure $ mconcat pureFolded <> mconcat impureFolded

runNatalQuery :: NatalArgs -> IO (Seq Event)
runNatalQuery (NatalArgs i@Interval{start,end} qs mqs ReferenceEvent{eventTime, eventLocation} houseSystem) = do
  Just ttStart <- toJulianDay start
  Just ttEnd   <- toJulianDay end
  Just ttEvent <- toJulianDay eventTime
  Just ut1Event <- toJulianDay eventTime
  referenceEphemeris <- readEphemerisEasy True ttEvent
  let (pureNatalQueries, impureNatalQueries) = NE.partition canCompose qs
      (pureMundaneQueries, impureMundaneQueries) = partition canCompose mqs
      ephe = streamEpheJDF ttStart ttEnd
  case referenceEphemeris of
    Left e -> fail e
    Right ref -> do
      CuspsCalculation{houseCusps} <- calculateCusps houseSystem ut1Event eventLocation
      let houses = zipWith House [I .. XII] houseCusps
      pureFolded :> _ <-
        ephe
        & ephemerisWindows 2
        & L.purely S.fold 
          (traverse (toFoldNatal ref houses) pureNatalQueries 
          <> traverse toFoldMundane pureMundaneQueries)

      impureFolded <- 
        traverse (execNatal i ref houses) impureNatalQueries
        <> traverse (execMundane i) impureMundaneQueries

      pure $ mconcat pureFolded <> mconcat impureFolded

-------------------------------------------------------------------------------
toFoldMundane :: MundaneQuery -> L.Fold (Seq (Ephemeris Double)) (Seq Event)
toFoldMundane =
  \case
    -- TODO(luis) use planets to filter
    (QueryDirectionChange planets) ->
      L.foldMap getRetrogrades collapse
    (QueryZodiacIngress planets)  ->
      L.foldMap (getZodiacCrossings (toList planets) westernZodiacSigns) collapse
    (QueryPlanetaryMundaneTransit pairs) ->
      L.foldMap (getTransits (toList pairs)) collapse
    QueryLunarPhase ->
      L.foldMap mapLunarPhases getMerged
    -- All other queries are meant to be executed directly, without constructing a Fold
    _ -> mempty

execMundane :: Interval -> MundaneQuery -> IO (Seq Event)
execMundane Interval{start,end} =
  \case
    QueryEclipse -> do
     -- TODO: expose the function that can produce both at the same time
      Just utStart <- toJulianDay start
      Just utEnd   <- toJulianDay end
      ecl <- allEclipses utStart utEnd
      pure $ fromList $ map Eclipse ecl
    -- All other queries are meant to be folded.
    _ -> pure mempty

toFoldNatal :: Ephemeris Double -> [House] -> NatalQuery -> L.Fold (Seq (Ephemeris Double)) (Seq Event)
toFoldNatal ephe houses =
  \case
    (QueryHouseIngress planets) ->
      L.foldMap (getHouseCrossings (toList planets) houses) collapse
    (QueryPlanetaryNatalTransit pairs) ->
      L.foldMap (getNatalTransits ephe (toList pairs)) collapse
    (QueryCuspTransit pairs) ->
      let pairList = toList pairs
          chosenPlanets = map fst pairList
          chosenHouseNames  = nub $ map snd pairList
          chosenHouses = houses & filter (houseName >>> (`elem` chosenHouseNames))
      in L.foldMap (getCuspTransits chosenHouses chosenPlanets) collapse
    _ -> mempty

execNatal :: Interval -> Ephemeris Double -> [House] -> NatalQuery -> IO (Seq Event)
execNatal Interval{start,end} ephe houses =
  \case
    (QueryLunarNatalTransit planets) -> do
      Just ttStart <- toJulianDay start
      Just ttEnd   <- toJulianDay end
      lun <- selectLunarTransits ttStart ttEnd ephe (toList planets)
      pure $ collapse lun
    (QueryLunarCuspTransit houseNames) -> do
      Just ttStart <- toJulianDay start
      Just ttEnd   <- toJulianDay end
      let chosenHouses = houses & filter (houseName >>> (`elem` houseNames))
      lunCusps <- selectLunarCuspTransits ttStart ttEnd chosenHouses
      pure $ collapse lunCusps
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
