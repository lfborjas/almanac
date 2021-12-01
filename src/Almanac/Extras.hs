{-# LANGUAGE NamedFieldPuns #-}
module Almanac.Extras where

import Almanac.Event.Types
import Data.Bifunctor (first, Bifunctor (bimap))
import Data.Foldable (foldMap')
import Data.Function
import qualified Data.Map as M
import qualified Data.Sequence as Sq
import Data.Time
import SwissEphemeris
import Data.List (tails)
import Data.Tuple (swap)
import Control.Applicative (liftA2)

-- | Did we find at least one moment of exactitude for the given Event?
hasExactitude :: ExactEvent -> Bool
hasExactitude (ExactEvent _ xs) = not . null $ xs

-- | Return a 'Map' of events indexed by 'Day' in a given 'TimeZone'
indexByDay :: TimeZone -> Sq.Seq ExactEvent -> M.Map Day (Sq.Seq ExactEvent)
indexByDay tz events =
  foldMap' repeatPerEvent events
    & map (first getDay)
    & M.fromListWith (<>)
  where
    repeatPerEvent e@(ExactEvent _evt exacts) =
      zip (map (utcToZonedTime tz) exacts) (repeat $ Sq.singleton e)
    getDay (ZonedTime (LocalTime d _tod) _tz) = d

-- | All distinct pairings of  planets, with the one that's faster
-- on average as the first of the pair, always.
uniquePairs :: [(Planet, Planet)]
uniquePairs =
  [(p1, p2) | (p1:ps) <- tails defaultPlanets, p2 <- ps]

allCuspPairs :: [(Planet, HouseName)]
allCuspPairs = liftA2 (,) defaultPlanets [I .. XII]

{-
The default sorting of planets here was obtained by looking at 100 years of average speeds
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

filteredPairs :: Eq a => [(Planet, a)] -> [Planet] -> [a] -> [(Planet, a)]
filteredPairs pairs transiting transited =
  pairs
  & filter (uncurry (&&) . bimap (`elem` transiting) (`elem` transited))

westernZodiacSigns :: [Zodiac]
westernZodiacSigns =
  zipWith3 Zodiac [Aries .. Pisces] zodiacs (map (+30) zodiacs)
  where
    zodiacs = take 12 $ iterate (+ 30) 0

-- | When looking at natal transits, you _probably_ want all planets
-- including slow+fast pairs and pairs with themselves. You may
-- not want the Moon, since it moves very fast and daily ephemeris
-- analysis won't catch _all_ of its transits. That's why 'QueryLunarTransit'
-- is provided as a separate query (you can always include the moon
-- in planetary transits, nothing will explode--it just may not show up)
defaultNatalTransitPairs :: [(Planet, Planet)]
defaultNatalTransitPairs = 
  filteredPairs 
    allPairs
    (tail defaultPlanets)
    defaultPlanets

-- | When looking at mundane transits, you're very like to want to
-- see fast planets transiting slow planets, sans the moon.
defaultMundaneTransitPairs :: [(Planet, Planet)]
defaultMundaneTransitPairs =
  filteredPairs
    uniquePairs
    (tail defaultPlanets)
    defaultPlanets
    
-- | When looking at cusp transits, it's very likely you don't
-- want to look at the Moon (there's a special query for her
-- 'QueryLunarCuspTransit':) this is because we use a faster
-- interpolation to find lunar intersections, vs. trying to
-- examine daily increments (which the Moon may evade!)
defaultCuspTransiting :: [HouseName] -> [(Planet, HouseName)]
defaultCuspTransiting =
  filteredPairs
    allCuspPairs
    (tail defaultPlanets)

-- | In most astrological applications, only transits with
-- the Ascendant and MC are examined
defaultHouses :: [HouseName]
defaultHouses = [I, X]

-- | In most astrological applications, only transits with
-- the Ascendant and MC are examined, so we provide
-- a list of pairs of all planets to those
defaultCuspTransitPairs :: [(Planet, HouseName)]
defaultCuspTransitPairs = defaultCuspTransiting defaultHouses

-- major aspects
sextile, square, trine, opposition, conjunction :: Aspect
conjunction = Aspect Conjunction 0 5 5
sextile = Aspect Sextile 60 5 5
square = Aspect Square 90 5 5
trine = Aspect Trine 120 5 5
opposition = Aspect Opposition 180 5 5

-- minor aspects
semiSextile, semiSquare, quintile, sesquisquare, biQuintile, quincunx :: Aspect
semiSextile = Aspect SemiSextile 30 3 3
semiSquare = Aspect SemiSquare 45 3 3
quintile = Aspect Quintile 72 2 2
sesquisquare = Aspect Sesquisquare 135 3 3
biQuintile = Aspect BiQuintile 144 2 2
quincunx = Aspect Quincunx 150 3 3

-- per: https://en.wikipedia.org/wiki/Astrological_aspect#/media/File:12_astrological_aspects.png
majorAspects :: [Aspect]
majorAspects = [conjunction, semiSextile, sextile, square, trine, quincunx, opposition]

defaultAspects :: [Aspect]
defaultAspects = 
  [
    conjunction,
    semiSextile,
    semiSquare,
    sextile,
    quintile,
    square,
    trine,
    sesquisquare,
    biQuintile,
    quincunx,
    opposition
  ]
