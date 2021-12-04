{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Almanac.Event.Types where

import SwissEphemeris
    ( HasEclipticLongitude(..),
      LunarEclipseType,
      LunarPhaseName,
      Planet,
      PlanetMotion,
      SolarEclipseType,
      ZodiacSignName,
      JulianDayTT,
      JulianDayUT1 ) 
import Almanac.EclipticLongitude ( EclipticLongitude )
import qualified Data.Sequence as S
import Data.Sequence (ViewR(EmptyR, (:>)), ViewL (EmptyL, (:<)), (<|), (|>), (><))
import qualified Data.Map as M
import Data.Time (UTCTime)

-------------------------------------------------------------------------------
-- SPECIALIZED CONTAINTERS
-------------------------------------------------------------------------------

-- | When two values of the same type are encountered, in a situation
-- where they may end up contiguous in a monoid, determine how the encounter
-- is to be treated: keep both? Discard? Update one based on info carried by the other?
-- Produce a third value that replaces either or both? See 'MergeStrategy'.
class Merge a where
  merge :: a -> a -> MergeStrategy a
  
class IsEclipticBand a where
  eclipticStart :: a -> Double
  eclipticEnd :: a -> Double

data MergeStrategy a
  = ReplaceBoth a a
  | ReplaceL a
  | ReplaceR a
  | Merge a
  | KeepBoth
  deriving (Eq, Show)

instance Functor MergeStrategy where
  fmap alpha (ReplaceBoth x y) = ReplaceBoth (alpha x) (alpha y)
  fmap alpha (ReplaceL x)      = ReplaceL (alpha x)
  fmap alpha (ReplaceR y)      = ReplaceR (alpha y)
  fmap alpha (Merge    z)      = Merge    (alpha z)
  fmap _     KeepBoth          = KeepBoth

-- | A sequence where appending may result in the elements at the
-- "append point" to be updated or replaced.
newtype MergeSeq a =
  MergeSeq {getMerged :: S.Seq a}
  deriving stock (Show)
  deriving newtype Eq
  deriving Foldable via S.Seq

singleton :: a -> MergeSeq a
singleton = MergeSeq . S.singleton

instance Merge a => Semigroup (MergeSeq a) where
  (MergeSeq s1) <> (MergeSeq s2) =
    MergeSeq $ doMerge s1Last s2First
    where
      s1Last  = S.viewr s1
      s2First = S.viewl s2
      doMerge EmptyR EmptyL    = mempty
      doMerge EmptyR (x :< xs) = x <| xs
      doMerge (xs :> x) EmptyL = xs |> x
      doMerge (xs :> x) (y :< ys) =
        case merge x y of
          ReplaceBoth a b -> (xs |> a) >< (b <| ys)
          Merge       a   -> (xs |> a) >< ys
          ReplaceL    a   -> (xs |> a) >< (y <| ys)
          ReplaceR      b -> (xs |> x) >< (b <| ys)
          KeepBoth        -> (xs |> x) >< (y <| ys)

instance Merge a => Monoid (MergeSeq a) where
  mempty = MergeSeq mempty

-- newtype somewhat inspired by:
-- https://stackoverflow.com/questions/32160350/folding-over-a-list-and-counting-all-occurrences-of-arbitrarily-many-unique-and
newtype Aggregate grouping row =
  Aggregate { getAggregate :: M.Map grouping row}
  deriving stock (Show)

instance (Ord k, Semigroup v) => Semigroup (Aggregate k v) where
  (Aggregate a1) <> (Aggregate a2) =
    Aggregate $ M.unionWith (<>) a1 a2

-- NOTE: we _could_ use DerivingVia to automatically derive
-- this trivial instance... except that it seems to re-define
-- `(<>)`, tossing away the one we define for @Semigroup@, above.
instance (Ord k, Semigroup v) => Monoid (Aggregate k v) where
  mempty = Aggregate mempty

-- | An aggregate where each row is a merge of results
type Grouped a b = Aggregate a (MergeSeq b)

-------------------------------------------------------------------------------
-- EVENTS
-------------------------------------------------------------------------------

data Event
  = DirectionChange  PlanetStation
  | ZodiacIngress    (Crossing Zodiac)
  | HouseIngress     (Crossing House)
  | PlanetaryTransit (Transit Planet)
  | HouseTransit     (Transit House)
  | LunarPhase       LunarPhaseInfo
  | Eclipse          EclipseInfo
  deriving (Eq, Show)

type EventSeq = MergeSeq Event

-- | Events that travel with their moments of exactitude
-- (stored as a separate datum since they usually require
-- further trips through IO beyond just traversing an ephemeris.)
data ExactEvent = 
  ExactEvent {
    event :: Event,
    exactitudeMoments :: [UTCTime]
  } deriving (Eq, Show)

-- | Alias for readability
getEvents :: EventSeq -> S.Seq Event
getEvents = getMerged

instance Merge Event where
  merge (DirectionChange a) (DirectionChange b) = 
    DirectionChange <$> (a `merge` b)
  merge (ZodiacIngress a) (ZodiacIngress b) =
    ZodiacIngress <$> (a `merge` b)
  merge (HouseIngress a) (HouseIngress b) =
    HouseIngress <$> (a `merge` b)
  merge (PlanetaryTransit a) (PlanetaryTransit b) =
    PlanetaryTransit <$> (a `merge` b)
  merge (HouseTransit a) (HouseTransit b) =
    HouseTransit <$> (a `merge` b)
  merge (LunarPhase a) (LunarPhase b) =
    LunarPhase <$> (a `merge` b)
  merge (Eclipse a) (Eclipse b) =
    Eclipse <$> (a `merge` b)
  merge _ _ = KeepBoth 

data Station
  = StationaryRetrograde
  | StationaryDirect
  | Retrograde
  | Direct
  deriving (Eq, Show)
 
-------------------------------------------------------------------------------
data PlanetStation = PlanetStation
  { stationStarts :: !JulianDayTT
  , stationEnds :: !JulianDayTT
  , stationType :: !Station
  , stationPlanet :: !Planet
  }
  deriving (Eq, Show)

instance Merge PlanetStation where
  x `merge` y =
    if stationType y == stationType x && stationPlanet x == stationPlanet y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        stationStarts = stationStarts x
      , stationEnds   = stationEnds y
      , stationType   = stationType y
      }

-------------------------------------------------------------------------------

data Crossing crossed = Crossing
  { crossingStarts :: !JulianDayTT
  , crossingEnds :: !JulianDayTT
  , crossingCrosses :: !crossed
  , crossingPlanet :: !Planet
  , crossingDirection :: !PlanetMotion
  } deriving (Eq, Show)

instance Eq a => Merge (Crossing a) where
  x `merge` y =
    if crossingCrosses x == crossingCrosses y && crossingPlanet x == crossingPlanet y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        crossingEnds = crossingEnds y,
        crossingDirection = crossingDirection y
      }

data Zodiac = Zodiac
  { signName :: ZodiacSignName, signStart :: Double, signEnd :: Double }
  deriving (Eq, Show)

instance HasEclipticLongitude Zodiac where
  getEclipticLongitude (Zodiac _ l _) = l
  setEclipticLongitude (Zodiac a _ c) l' = Zodiac a l' c  
  
instance IsEclipticBand Zodiac where
  eclipticStart = signStart
  eclipticEnd = signEnd

data HouseName
  = I
  | II
  | III
  | IV
  | V
  | VI
  | VII
  | VIII
  | IX
  | X
  | XI
  | XII
  deriving (Eq, Show, Enum, Ord)

data House = House
  { houseName :: HouseName, houseCusp :: Double, houseEnd :: Double }
  deriving (Eq, Show)
  
instance HasEclipticLongitude House where
  getEclipticLongitude = houseCusp
  setEclipticLongitude (House n _c e) c' = House n c' e
  
instance IsEclipticBand House where
  eclipticStart = houseCusp
  eclipticEnd = houseEnd

instance Ord House where
  a `compare` b = compare (houseCusp a) (houseCusp b)

-------------------------------------------------------------------------------

data Relation
  = Below
  | Crossed
  | Above
  deriving (Eq, Show)

data TransitPhaseName
  = ApplyingDirect
  | ApplyingRetrograde
  | TriggeredDirect
  | TriggeredRetrograde
  | SeparatingDirect
  | SeparatingRetrograde
  deriving (Eq, Show)

data AspectName
  = Conjunction
  | Sextile
  | Square
  | Trine
  | Opposition
  | Quincunx
  | SemiSextile
  | Quintile
  | BiQuintile
  | Septile
  | SemiSquare
  | Novile
  | Sesquisquare -- Trioctile
  deriving (Eq, Enum, Show)

data Aspect = Aspect {
  aspectName :: !AspectName
, angle :: !Double
, orbApplying :: !Double
, orbSeparating :: !Double
} deriving (Eq, Show)

data TransitPhase = TransitPhase {
  phaseName :: !TransitPhaseName
, phaseStarts :: !JulianDayTT
, phaseEnds :: !JulianDayTT
} deriving (Eq, Show)

instance Merge TransitPhase where
  x `merge` y =
    if phaseName x == phaseName y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        phaseEnds = phaseEnds y
      }

data Transit over = Transit {
  aspect :: !AspectName
, transiting :: !Planet
, transited :: !over
, transitAngle :: !Double
, transitOrb :: !Double
, transitStarts :: !JulianDayTT
, transitEnds :: !JulianDayTT
, transitPhases :: !(MergeSeq TransitPhase)
, transitIsExact :: ![JulianDayTT]
, transitCrosses :: !EclipticLongitude
} deriving (Eq, Show)

instance Eq a => Merge (Transit a) where
  x `merge` y =
    if aspect x == aspect y && transiting x == transiting y && transited x == transited y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
          transitEnds = transitEnds y,
          transitAngle = transitAngle y,
          transitOrb = transitOrb y,
          transitPhases = transitPhases x <> transitPhases y
        }

-------------------------------------------------------------------------------

data LunarPhaseInfo = LunarPhaseInfo
  { lunarPhaseName :: !LunarPhaseName
  , lunarPhaseStarts :: !JulianDayTT
  , lunarPhaseEnds :: !JulianDayTT
  } deriving (Eq, Show)

instance Merge LunarPhaseInfo where
  x `merge` y =
    if lunarPhaseName x == lunarPhaseName y then
      Merge merged
    else
      KeepBoth
    where
      merged = x {
        lunarPhaseEnds = lunarPhaseEnds y
      }

-------------------------------------------------------------------------------

data EclipseInfo
  = SolarEclipse !SolarEclipseType !JulianDayUT1 
  | LunarEclipse !LunarEclipseType !JulianDayUT1
  deriving (Eq, Show)
 
instance Merge EclipseInfo where
  merge _ _= KeepBoth 
