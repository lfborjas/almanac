
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLists #-}

module Almanac.Event.Transit where

import Data.Sequence (Seq ((:<|)), fromList)
import qualified Data.Map as M
import SwissEphemeris
    ( HasEclipticLongitude(getEclipticLongitude),
      Planet(Pluto, Moon, Mercury, Venus, Sun, Mars, Jupiter, MeanApog,
             Saturn, MeanNode, Chiron, Uranus, Neptune), JulianDayTT, moonCrossingBetween, mkJulianDay, SingTimeStandard (STT), JulianDay (getJulianDay) )
import SwissEphemeris.Precalculated
    ( planetEphe,
      Ephemeris(epheDate, ephePositions),
      EphemerisPosition(epheLongitude, epheSpeed, ephePlanet) )
import Data.Fixed (mod')
import Control.Monad (guard, join)
import Data.List (tails, nub)
import Control.Applicative (liftA2)
import Almanac.EclipticLongitude ( EclipticLongitude(..), (<->), toEclipticLongitude )
import Data.Function
import Almanac.Event.Types
import Data.Foldable (foldMap')
import Data.Either (rights)
import Data.Tuple (swap)
import Almanac.Import (concatForEach)
import Data.Bifunctor (bimap)
import Almanac.Event.PlanetStation

type EphemerisPoint = (JulianDayTT, EphemerisPosition Double)

getTransits :: [(Planet, Planet)] -> Seq (Ephemeris Double) -> Grouped (Planet, Planet) Event
getTransits chosenPairs (day1Ephe :<| day2Ephe :<| _) =
  concatForEach chosenPairs $ \pair@(planet1, planet2) ->
      let planet1Ephe1 = (epheDate day1Ephe,) <$> planetEphe planet1 day1Ephe
          planet1Ephe2 = (epheDate day2Ephe,) <$> planetEphe planet1 day2Ephe
          planet2Ephe2 = (epheDate day2Ephe,) <$> planetEphe planet2 day2Ephe
          planet1Ephes = liftA2 (,) planet1Ephe1 planet1Ephe2
          transit' = join $ mkTransit ephePlanet <$> planet1Ephes <*> planet2Ephe2
      in case transit' of
        Nothing -> mempty
        Just transit -> Aggregate $ M.fromList [(pair, singleton $ PlanetaryTransit transit)]

getTransits _ _ = mempty


getNatalTransits :: Ephemeris Double -> [(Planet, Planet)] -> Seq (Ephemeris Double) -> Grouped (Planet, Planet) Event
getNatalTransits natalEphemeris chosenPairs (day1Ephe :<| day2Ephe :<| _) =
  concatForEach chosenPairs $ \pair@(planet1, planet2) ->
      let planet1Ephe1 = (epheDate day1Ephe,) <$> planetEphe planet1 day1Ephe
          planet1Ephe2 = (epheDate day2Ephe,) <$> planetEphe planet1 day2Ephe

          planet2Ephe2 = (epheDate day2Ephe,) <$> staticPosition (planetEphe planet2 natalEphemeris)
          planet1Ephes = liftA2 (,) planet1Ephe1 planet1Ephe2
          transit' = join $ mkTransit ephePlanet <$> planet1Ephes <*> planet2Ephe2
      in case transit' of
        Nothing -> mempty
        Just transit -> Aggregate $ M.fromList [(pair, singleton $ PlanetaryTransit transit)]

getNatalTransits _ _ _ = mempty

getCuspTransits :: [House] -> [Planet] -> Seq (Ephemeris Double) -> Grouped (Planet, House) Event
getCuspTransits cusps chosenPlanets (day1Ephe :<| day2Ephe :<| _) =
  concatForEach (cProduct chosenPlanets cusps )$ \pair@(planet1, cusp) ->
      let planet1Ephe1 = (epheDate day1Ephe,) <$> planetEphe planet1 day1Ephe
          planet1Ephe2 = (epheDate day2Ephe,) <$> planetEphe planet1 day2Ephe

          planet2Ephe2 = Just (epheDate day2Ephe,cusp)
          planet1Ephes = liftA2 (,) planet1Ephe1 planet1Ephe2
          transit' = join $ mkTransit id <$> planet1Ephes <*> planet2Ephe2
      in case transit' of
        Nothing -> mempty
        Just transit -> Aggregate $ M.fromList [(pair, singleton $ HouseTransit transit)]

getCuspTransits _ _ _ = mempty

cProduct :: [Planet] -> [a] -> [(Planet, a)]
cProduct ps as = [(p,a) | p <- ps, a <- as]

staticPosition :: Maybe (EphemerisPosition Double) -> Maybe (EphemerisPosition Double)
staticPosition (Just pos) = Just $ pos{epheSpeed = 0.0}
staticPosition Nothing = Nothing

-- | All distinct pairings of  planets, with the one that's faster
-- on average as the first of the pair, always.
uniquePairs :: [(Planet, Planet)]
uniquePairs =
  [(p1, p2) | (p1:ps) <- tails defaultPlanets, p2 <- ps]

{-
The default sorting of planets here was obtained by looking at 100 years of average speeds:
cabal new-run laboratorium -- query -q "Averages" -s "1989-01-01" -e "2089-01-01" --ephe-path "./ephe"
Up to date
[(Moon,13.176522281580842),
(Mercury,1.2173611188617248),
(Venus,1.042309783743218),
(Sun,0.9856478045400626),
(Mars,0.5679595888524764),
(Jupiter,0.13204562470426282),
(MeanApog,0.11140269708380175),
(Saturn,6.881223337573507e-2),
(MeanNode,5.295424163793801e-2),
(Chiron,5.2216388904251725e-2),
(Uranus,3.229203526261477e-2),
(Neptune,2.112966146937543e-2),
(Pluto,2.060110471243601e-2)]
-}

defaultPlanets :: [Planet]
defaultPlanets =
      [ Moon
      , Mercury
      , Venus
      , Sun
      , Mars
      , Jupiter
      , MeanApog
      , Saturn
      , MeanNode
      , Chiron
      , Uranus
      , Neptune
      , Pluto
      ]

slowPlanets :: [Planet]
slowPlanets =
  [ Jupiter
  , MeanApog
  , Saturn
  , MeanNode
  , Chiron
  , Uranus
  , Neptune
  , Pluto
  ]

-- All pairs, including a planet with itself and slow planets transiting fast ones (vs the usual,
-- fast ones over slow ones) -- useful for natal transits
allPairs :: [(Planet, Planet)]
allPairs =
  uniquePairs <> map swap uniquePairs <> selfPairs
  where
    selfPairs = zip defaultPlanets defaultPlanets

filteredPairs :: [(Planet, Planet)] -> [Planet] -> [Planet] -> [(Planet, Planet)]
filteredPairs pairs transiting transited =
  pairs
  & filter (uncurry (&&) . bimap (`elem` transiting) (`elem` transited))

sextile, square, trine, opposition, conjunction :: Aspect
conjunction = Aspect Conjunction 0 5 5
sextile = Aspect Sextile 60 5 5
square = Aspect Square 90 5 5
trine = Aspect Trine 120 5 5
opposition = Aspect Opposition 180 5 5

-- | Aspects to consider, arranged in a "cycle":
-- as soon as one is determined, no need to seek the others.
aspects :: [Aspect]
aspects = [conjunction, sextile, square, trine, opposition]


mkTransit
  :: HasEclipticLongitude a
  =>  (a -> b)
  -> (EphemerisPoint, EphemerisPoint)
  -- ^ planet 1 at days 1->2
  -> (JulianDayTT, a)
  -- ^ planet 2 at day 2
  -> Maybe (Transit b)
mkTransit ins transiting@((t1, p11), (t2, p12)) (_t22, p22)
  = do
    let (before, after, transitedPos) = (epheLongitude p11, epheLongitude p12, getEclipticLongitude p22)
    (aspectName, angle', orb', meets) <- determineAspect after transitedPos
    -- TODO(luis) use diffdeg2n:
    -- https://github.com/lfborjas/swiss-ephemeris/blob/7edfb51ed12b301ffb44811c472f864a49cc7f16/csrc/swephlib.c#L3824 
    let (before', after', ref) = normalize (before, after, getEclipticLongitude meets)
        station = movement transiting
        rel = relation before' after' ref
        phase = transitPhase station rel
        phaseInfo = singleton $ TransitPhase phase t1 t2
    pure
      Transit {
        aspect = aspectName,
        transiting = ephePlanet p11,
        transited = ins p22,
        transitAngle = angle',
        transitOrb = orb',
        transitStarts = t1,
        transitEnds = t2,
        transitPhases = phaseInfo,
        transitIsExact = mempty,
        transitCrosses = meets
      }



-------------------------------------------------------------------------------
-- UTILS
-------------------------------------------------------------------------------

-- | Given two moments of movement for two separate planets/body,
-- determine which is to be considered the "fastest"
isTransiting :: EphemerisPoint -> EphemerisPoint -> Bool
isTransiting (_,p12) (_,p22) =
  absSpeed p22 <= absSpeed p12

absSpeed :: EphemerisPosition Double -> Double
absSpeed = abs . epheSpeed

-- | Given two moments of movement for a given planet/body,
-- determine the "character" thereof
movement :: (EphemerisPoint, EphemerisPoint) -> Station
movement (_d1@(_t1, _p1), _d2@(_t2, p2))
  | isRelativelyStationary p2 =
    if signum (epheSpeed p2) > 0 then
      StationaryDirect
    else
      StationaryRetrograde
  | signum (epheSpeed p2) > 0 = Direct
  | otherwise = Retrograde

-- | Given two moments of a moving planet, and a reference point
-- determine if the moving planet remained above, below or crossed the
-- reference point
relation :: Double -> Double -> Double -> Relation
relation p1 p2 ref
  |  p1 <  ref &&  p2 <  ref = Below
  |  p1 <= ref &&  p2 >= ref = Crossed
  |  p1 >= ref &&  p2 <= ref = Crossed
  |  p1 >  ref &&  p2 >  ref = Above
  | otherwise = Crossed


transitPhase :: Station -> Relation -> TransitPhaseName
transitPhase Direct Below = ApplyingDirect
transitPhase StationaryDirect Below = ApplyingDirect
transitPhase Direct Crossed = TriggeredDirect
transitPhase StationaryDirect Crossed = TriggeredDirect
transitPhase Direct Above = SeparatingDirect
transitPhase StationaryDirect Above = SeparatingDirect
transitPhase Retrograde Above = ApplyingRetrograde
transitPhase StationaryRetrograde Above = ApplyingRetrograde
transitPhase Retrograde Crossed = TriggeredRetrograde
transitPhase StationaryRetrograde Crossed = TriggeredRetrograde
transitPhase Retrograde Below = SeparatingRetrograde
transitPhase StationaryRetrograde Below = SeparatingRetrograde

determineAspect :: Double -> Double -> Maybe (AspectName, Double, Double, EclipticLongitude)
determineAspect p1 p2 =
  headMaybe $ aspectCycle (EclipticLongitude p1) (EclipticLongitude p2)

aspectCycle :: EclipticLongitude -> EclipticLongitude -> [(AspectName, Double, Double, EclipticLongitude)]
aspectCycle p1 p2 = do
  asp <- aspects
  let dist = p1 <-> p2
      theta = angle asp
      orb = abs $ theta - dist
      crossA = p2 + EclipticLongitude theta
      crossB = p2 - EclipticLongitude theta
      crossesAt =
        if p1 <-> crossA <= p1 <-> crossB then crossA else crossB
  guard $ abs (theta - dist) <= maxOrb asp
  pure (aspectName asp, dist, orb, crossesAt)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe xs = Just . head $ xs

clampCircle :: Double -> Double
clampCircle n =
  rectified `mod'` 360
  where
    rectified = if n < 0 then n + 360 else n

maxOrb :: Aspect -> Double
maxOrb Aspect{orbApplying, orbSeparating} = max orbApplying orbSeparating

-- | Given two points that relate to a third point, translate
-- by 90 degrees if the line jumps over the 360/0 point
normalize :: (Double, Double, Double) -> (Double, Double, Double)
normalize points@(p1, p2, ref)
  | abs (p2 - p1) > 30 = (translate p1, translate p2, translate ref)
  | otherwise = points
  where
    translate = (+90)

selectLunarTransits :: JulianDayTT -> JulianDayTT -> Ephemeris Double -> [Planet] -> IO (Grouped (Planet, Planet) Event)
selectLunarTransits start end natalEphemeris chosenPlanets =
  foldMap' mkLunarTransit (ephePositions natalEphemeris)
  where
    mkLunarTransit :: EphemerisPosition Double -> IO (Grouped (Planet, Planet) Event)
    mkLunarTransit pos = do
      if ephePlanet pos `elem` chosenPlanets then do
        transit <- lunarAspects ephePlanet start end pos --(EclipticLongitude . epheLongitude $ pos)
        if null transit then
          mempty
        else
          pure $ Aggregate $ M.fromList [((Moon, ephePlanet pos), MergeSeq . fromList . map PlanetaryTransit $ transit)]
      else
        mempty

selectLunarCuspTransits :: JulianDayTT -> JulianDayTT -> [House]-> IO (Grouped (Planet, House) Event)
selectLunarCuspTransits start end =
  foldMap' mkLunarTransit
  where
    mkLunarTransit :: House -> IO (Grouped (Planet, House) Event)
    mkLunarTransit pos = do
      transit <- lunarAspects id start end pos
      if null transit then
        mempty
      else
        pure $ Aggregate $ M.fromList [((Moon, pos), MergeSeq . fromList . map HouseTransit $ transit)]


lunarAspects
  :: HasEclipticLongitude a
  => (a -> b)
  -> JulianDayTT
  -> JulianDayTT
  -> a
  -> IO [Transit b]
lunarAspects ins start end pos =
  foldMap' crossings aspects
  where
    crossings Aspect{aspectName, angle} = do
      let crossA = toEclipticLongitude pos + EclipticLongitude angle
          crossB = toEclipticLongitude pos - EclipticLongitude angle
      crossesA <- moonCrossingBetween (getEclipticLongitude crossA) start end
      crossesB <- moonCrossingBetween (getEclipticLongitude crossB) start end
      pure
        . map (toTransit aspectName angle)
        . nub
        . rights
        $ [(,crossA) <$> crossesA, (,crossB) <$> crossesB]

    toTransit aspname angl (exactCrossingTime, exactCrossingLng) =
      Transit {
        aspect = aspname,
        transiting = Moon,
        transited = ins pos,
        transitAngle = angl,
        transitOrb = 0,
        transitStarts = estimateStart exactCrossingTime,
        transitIsExact = [exactCrossingTime],
        transitEnds = estimateEnd exactCrossingTime,
        transitPhases = mempty,
        transitCrosses = exactCrossingLng
      }
    -- from:
    -- https://github.com/aloistr/swisseph/blob/40a0baa743a7c654f0ae3331c5d9170ca1da6a6a/sweph.c#L8494
    meanLunarSpeed = 360.0 / 27.32
    -- given the mean lunar speed, estimate when the Moon was 5 degrees before
    -- and 5 degrees after the moment of exact crossing
    estimateStart t' =
      mkJulianDay STT tBefore
      where
        tBefore = getJulianDay t' - 5/meanLunarSpeed
    estimateEnd t' =
      mkJulianDay STT tAfter
      where
        tAfter = getJulianDay t' + 5/meanLunarSpeed
