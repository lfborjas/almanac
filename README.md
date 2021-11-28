# Almanac

Utilities for processing pre-calculated Ephemeris data (
see [swiss-ephemeris](https://github.com/lfborjas/swiss-ephemeris).) 
Given an interval and a set of queries, it will efficiently traverse ephemeris in daily
increments (plus more direct interpolation for the Moon, since daily
traversal is too coarse for its average speed,) and return a [`Seq`](https://hackage.haskell.org/package/containers)
of `Events`. One can then choose to inspect the extracted events to calculate when _exactly_
they happen -- this is recommended as an extra step, only on demand, since exactitude calculation
needs to do some numerical interpolation, vs. simple fast daily perusal.

This library was incubated in my 
[laboratorium](https://natal-chart/laboratorium), with some further refinements.

To use this, you'll have to get ahold of ephemeris files (see my `swiss-ephemeris`, linked
above, for info on where to find them;) you'll also need _precalculated_ ephemeris, which
are not officially distributed by astro.com, but I've put a couple of centuries of precalculated
ephemeris in this project's `test/ephe/` directory (they're _tiny_ files.) Functions to generate
precalculated ephemeris are also [provided by `swiss-ephemeris`](https://hackage.haskell.org/package/swiss-ephemeris-1.4.0.0/docs/SwissEphemeris-Precalculated.html#g:9), here's some example usage
[from `laboratorium`](https://github.com/natal-chart/laboratorium/blob/56bb1be81dc8ce0b7f5ee44f0b0d269f50ef59a2/src/PrecalculatedEphemeris.hs#L19-L31), which can be run
as a CLI.

With those in place, here's some example usage (taken from the `test`s themselves):

```haskell
import Almanac
import Data.Sequence (Seq)
import Data.Time (UTCTime(..))
import System.Directory (makeAbsolute)

lunarPhasesAndEclipses :: IO (Seq ExactEvent)
lunarPhasesAndEclipses = do
  let nov2021 = UTCTime (fromGregorian 2021 11 1) 0
      dec2021 = UTCTime (fromGregorian 2021 12 1) 0
      q = mundane
            (Interval nov2021 dec2021)
            [QueryLunarPhase, QueryEclipse]
  runQuery q >>= eventsWithExactitude
  
-- NOTE: as mentioned in @swiss-ephemeris@, in a production application
-- you're better off setting environment variables with the location of the
-- ephemeris directory --  it's unnecessary and potentially thread-unsafe
-- in the presence of laziness to set them programmatically.
main :: IO ()
  fullEphePath <- makeAbsolute "./my-ephe"
  -- location of ephe data
  setEphemeridesPath fullEphePath
  -- location of precalculated ephe
  setEphe4Path fullEphePath

  print lunarPhasesAndEclipses
```

See the `test` directory for more examples: planetary ingresses, changes of direction, transits and queries
relevant to a "natal" reference event are provided.
