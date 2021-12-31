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
  TransitOptions(..),
  -- * Query construction
  mundane,
  natal,
  easyTransitOptions,
  -- * Query execution
  runQuery
) where

import Data.Time (UTCTime)
import Data.List ( nub )
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
import Data.Either (partitionEithers)
-- NOTE(luis) this covers the more general use case, so making an exception;
-- but we could technically require the query to specify _which_ signs it wants
-- to use, which would allow usage of Nakshatras, too
import Almanac.Extras (westernZodiacSigns)


-- | Internal utility typeclass to signal how a query is to be processed:
-- * A 'Composable' query can be turned into a @Fold@ which can then
--   be combined with others through a @Fold's@ 'Applicative' instance.
-- * A 'NonComposable' query must be executed directly, on its own. 
class QueryStrategy a where
  classifyQuery :: a -> Either (NonComposable a) (Composable a)

-- | A query is 'Composable' if it can be turned into a Fold
-- for one-pass processing along other queries
newtype Composable q = Composable q
-- | A query is 'Direct' if it cannot be turned into a Fold
-- and must be executed on its own
newtype NonComposable q = NonComposable q

-- | Options that can be provided to determine which aspects, and what data,
-- to include in transit queries
data TransitOptions a = 
  TransitOptions
    { includeTransitProgress :: Bool
    , transitAspects :: NonEmpty Aspect
    , transitTargets :: NonEmpty a
    } deriving (Eq, Show)

-- | Defaults for the less commonly used 'TransitOptions', only have to provide
-- the desired aspects and transit pairs/targets
easyTransitOptions :: NonEmpty Aspect -> NonEmpty a -> TransitOptions a
easyTransitOptions = TransitOptions False

data MundaneQuery
  = QueryDirectionChange (NonEmpty Planet)
  | QueryZodiacIngress   (NonEmpty Planet)
  | QueryPlanetaryMundaneTransit (TransitOptions (Planet, Planet))
  | QueryLunarPhase
  | QueryEclipse
  deriving (Eq, Show)

instance QueryStrategy MundaneQuery where
  classifyQuery QueryEclipse = Left . NonComposable $ QueryEclipse
  classifyQuery q = Right . Composable $ q

data NatalQuery
  = QueryHouseIngress    (NonEmpty Planet)
  | QueryPlanetaryNatalTransit (TransitOptions (Planet, Planet))
  | QueryCuspTransit  (TransitOptions (Planet, HouseName))
  | QueryLunarNatalTransit (TransitOptions Planet)
  | QueryLunarCuspTransit  (TransitOptions HouseName)
  deriving (Eq, Show)

instance QueryStrategy NatalQuery where
  classifyQuery q@(QueryLunarNatalTransit _) = Left . NonComposable $ q
  classifyQuery q@(QueryLunarCuspTransit _) = Left . NonComposable $ q
  classifyQuery q = Right . Composable $ q

data Interval =
  Interval {
    start :: !UTCTime,
    end :: !UTCTime
  } deriving (Eq, Show)

data ReferenceEvent =
  ReferenceEvent {
    eventTime :: !UTCTime,
    eventLocation :: !GeographicPosition
  } deriving (Eq, Show)

data MundaneArgs =
  MundaneArgs {
    mInterval :: Interval,
    mQueries  :: NonEmpty MundaneQuery
  }

data NatalArgs =
  NatalArgs {
    nInterval :: Interval,
    nReferenceEvent :: ReferenceEvent,
    nQueries  :: NonEmpty NatalQuery,
    nMundaneQueries :: [MundaneQuery],
    nHouseSystem :: HouseSystem
  }

data Query
  = Mundane MundaneArgs
  | Natal   NatalArgs

runQuery :: Query -> IO (Seq Event)
runQuery (Mundane args) = runMundaneQuery args
runQuery (Natal   args) = runNatalQuery args

-- | Convenience constructor of a mundane query
mundane :: Interval -> NonEmpty MundaneQuery -> Query
mundane i qs = Mundane $ MundaneArgs i qs

-- | Convenience constructor of a natal query without additional
-- mundane queries, and the 'Placidus' house system.
natal :: Interval -> ReferenceEvent -> NonEmpty NatalQuery -> Query
natal i r qs = Natal $ NatalArgs i r qs [] Placidus
-------------------------------------------------------------------------------
-- QUERY UTILITIES
-------------------------------------------------------------------------------

runMundaneQuery :: MundaneArgs -> IO (Seq Event)
runMundaneQuery (MundaneArgs i@Interval{start,end} qs) = do
  Just ttStart <- toJulianDay start
  Just ttEnd   <- toJulianDay end
  let (impureQueries, pureQueries) = partitionQueries qs
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
runNatalQuery (NatalArgs i@Interval{start,end} ReferenceEvent{eventTime, eventLocation} qs mqs houseSystem) = do
  Just ttStart <- toJulianDay start
  Just ttEnd   <- toJulianDay end
  Just ttEvent <- toJulianDay eventTime
  Just ut1Event <- toJulianDay eventTime
  referenceEphemeris <- readEphemerisEasy True ttEvent
  let (impureNatalQueries, pureNatalQueries) = partitionQueries qs
      (impureMundaneQueries, pureMundaneQueries) = partitionQueries mqs
      ephe = streamEpheJDF ttStart ttEnd
  case referenceEphemeris of
    Left e -> fail e
    Right ref -> do
      CuspsCalculation{houseCusps} <- calculateCusps houseSystem ut1Event eventLocation
      let houses = zipWith3 House [I .. XII] houseCusps (tail houseCusps <> [head houseCusps])
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

partitionQueries :: (QueryStrategy q, Foldable m) => m q -> ([NonComposable q], [Composable q]) 
partitionQueries = 
    F.toList >>> map classifyQuery >>> partitionEithers


toFoldMundane :: Composable MundaneQuery -> L.Fold (Seq (Ephemeris Double)) (Seq Event)
toFoldMundane (Composable q) =
  case q of
    (QueryDirectionChange planets) ->
      L.foldMap (getRetrogrades (toList planets)) collapse
    (QueryZodiacIngress planets)  ->
      L.foldMap (getZodiacCrossings (toList planets) westernZodiacSigns) collapse
    (QueryPlanetaryMundaneTransit TransitOptions{includeTransitProgress, transitAspects, transitTargets}) ->
      L.foldMap (getTransits includeTransitProgress (toList transitAspects) (toList transitTargets)) collapse
    QueryLunarPhase ->
      L.foldMap mapLunarPhases getMerged
    -- All other queries are meant to be executed directly, without constructing a Fold.
    _ -> mempty

execMundane :: Interval -> NonComposable MundaneQuery -> IO (Seq Event)
execMundane Interval{start,end} (NonComposable q) =
  case q of
    QueryEclipse -> do
     -- TODO: expose the function that can produce both at the same time
      Just utStart <- toJulianDay start
      Just utEnd   <- toJulianDay end
      ecl <- allEclipses utStart utEnd
      pure $ fromList $ map Eclipse ecl
    -- All other queries are meant to be folded.
    _ -> pure mempty

toFoldNatal :: Ephemeris Double -> [House] -> Composable NatalQuery -> L.Fold (Seq (Ephemeris Double)) (Seq Event)
toFoldNatal ephe houses (Composable q) =
  case q of
    (QueryHouseIngress planets) ->
      L.foldMap (getHouseCrossings (toList planets) houses) collapse
    (QueryPlanetaryNatalTransit TransitOptions{includeTransitProgress, transitAspects, transitTargets}) ->
      L.foldMap (getNatalTransits ephe includeTransitProgress (toList transitAspects) (toList transitTargets)) collapse
    (QueryCuspTransit TransitOptions{includeTransitProgress, transitAspects, transitTargets}) ->
      let pairList = toList transitTargets 
          chosenPlanets = map fst pairList
          chosenHouseNames  = nub $ map snd pairList
          chosenHouses = houses & filter (houseName >>> (`elem` chosenHouseNames))
      in L.foldMap (getCuspTransits chosenHouses includeTransitProgress (toList transitAspects) chosenPlanets) collapse
    _ -> mempty

execNatal :: Interval -> Ephemeris Double -> [House] -> NonComposable NatalQuery -> IO (Seq Event)
execNatal Interval{start,end} ephe houses (NonComposable q) =
  case q of
    (QueryLunarNatalTransit TransitOptions{transitAspects, transitTargets}) -> do
      Just ttStart <- toJulianDay start
      Just ttEnd   <- toJulianDay end
      lun <- selectLunarTransits (toList transitAspects) ttStart ttEnd ephe (toList transitTargets)
      pure $ collapse lun
    (QueryLunarCuspTransit TransitOptions{transitAspects, transitTargets}) -> do
      Just ttStart <- toJulianDay start
      Just ttEnd   <- toJulianDay end
      let chosenHouses = houses & filter (houseName >>> (`elem` transitTargets))
      lunCusps <- selectLunarCuspTransits (toList transitAspects) ttStart ttEnd chosenHouses
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
