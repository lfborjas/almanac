# Almanac

Utilities for processing pre-calculated Ephemeris data (see [swiss-ephemeris](https://github.com/lfborjas/swiss-ephemeris).) 
Given an interval and a set of queries, it will efficiently traverse precalculated ephemeris in daily
increments, or do faster interpolation of regular ephemeris data for certain queries like moon events and eclipses, and return a [`Seq`](https://hackage.haskell.org/package/containers)
of `Events`. One can then choose to inspect the extracted events to calculate when _exactly_
they happen -- this is recommended as an extra step, only on demand, since exactitude calculation
needs to do some numerical interpolation, vs. simple fast daily perusal.

Since the main use case is to get a sequence of events and presumably examine them, `Lens`es and `Traversal`s are
provided in the `Almanac.Optics` module to help in the otherwise tiresome deep pattern matching that could ensue. 
Note that there's **no dependency** on any actual `lens`-like library, so you'll have to bring your own to actually do "lens stuff" with the provided optics. 
To illustrate how the provided optics can be brought to life with a lens library, the tests use `microlens`. I have only added the most
obvious lenses and traversals (no `Prism`s, since I don't foresee any "construction" happening and thus `Traversal`s do the job nicely
and don't need a dependency on `profunctors`.)


To use this library, you'll have to [get ahold of ephemeris files](https://github.com/lfborjas/swiss-ephemeris#ephemerides-files); 
you'll also need _precalculated_ ephemeris, which are not officially distributed by astro.com, but I've put a couple of centuries of precalculated
ephemeris in this project's `test/ephe/` directory that you can just copy (they're _tiny_ files.) Functions to generate
precalculated ephemeris are also [provided by `swiss-ephemeris`](https://hackage.haskell.org/package/swiss-ephemeris-1.4.0.0/docs/SwissEphemeris-Precalculated.html#g:9), here's some example usage
[from `laboratorium`](https://github.com/natal-chart/laboratorium/blob/56bb1be81dc8ce0b7f5ee44f0b0d269f50ef59a2/src/PrecalculatedEphemeris.hs#L19-L31), which can be run as a CLI.

With those in place, here's some example usage:

```haskell
import Almanac
import Almanac.Optics
import Control.Applicative 
import Data.Time 
-- You can use any `lens`-compatible library here.
import Lens.Micro
import System.Directory (makeAbsolute)

lunarPhasesAndEclipses :: IO [(String, UTCTime)]
lunarPhasesAndEclipses = do
  let nov2021 = UTCTime (fromGregorian 2021 11 1) 0
      dec2021 = UTCTime (fromGregorian 2021 12 1) 0
      q = mundane
            (Interval nov2021 dec2021)
            [QueryLunarPhase, QueryEclipse] 

  -- Get events, then calculate the exact moment they happen
  exactEvents <- runQuery q >>= eventsWithExactitude

  -- For each event, get the phase name (if it's a lunar phase,)
  -- or the eclipse type (only when it's a lunar eclipse,) as well
  -- as the moment of exactitude. Using optics to deal with the
  -- annoying pattern matching that would ensue.
  let digest = (summarize <$> exactEvents) ^.. traversed . _Just
      summarize evt = 
        let phaseName = show <$> evt ^? eventL._LunarPhaseInfo.lunarPhaseNameL
            eclType   = show <$> evt ^? eventL._EclipseInfo._LunarEclipseType
            exactAt   = evt ^? exactitudeMomentsL._head
        in liftA2 (,) (phaseName <|> eclType) exactAt
  pure digest  

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

  lunarPhasesAndEclipses >>= print
  
{- 
  Would print something like 
[
  ("WaningCrescent","2021-11-01T13:28:27.314121723175Z"),
  ("NewMoon","2021-11-04T21:14:36.684200763702Z"),
  ("WaxingCrescent","2021-11-08T02:31:28.868464529514Z"),
  ("FirstQuarter","2021-11-11T12:46:02.566146254539Z"),
  ("WaxingGibbous","2021-11-15T07:27:48.519482016563Z"),
  ("FullMoon","2021-11-19T08:57:27.984892129898Z"),
  ("PartialLunarEclipse","2021-11-19T09:02:55.849740207195Z")
  ("WaningGibbous","2021-11-23T12:50:27.58442312479Z"),
  ("LastQuarter","2021-11-27T12:27:40.648325085639Z")
]
-}
```

See the `test` directory for more examples: planetary ingresses, changes of direction, transits and queries
relevant to a "natal" reference event are provided.

This library was incubated in my 
[laboratorium](https://natal-chart/laboratorium), with some further refinements.
