module Almanac.Optics where

import SwissEphemeris
import Almanac.Internal.Lens
import Almanac.Event.Types
import Data.Time (UTCTime)
{- | 
This module provides optics into queries and events. @Control.Lens@-compatible
lenses and traversals are provided (prisms would also be possible,
but that'd introduce a dependency on @profunctors@).

In keeping with less magical lens approaches (e.g. Relude or RIO,) instead
of lenses having the more obvious name and types having to carry an underscore
prefix, we suffix lenses here with @L@. Traversals do follow the less
contentious tradition from @Control.Lens@ and are prefixed with underscore.
-}

-------------------------------------------------------------------------------
-- Lenses for 'PlanetStation'
-------------------------------------------------------------------------------

stationStartsL :: Lens' PlanetStation JulianDayTT
stationStartsL =
  simpleLens get set
  where
    get = stationStarts
    set s start' = s{stationStarts = start'}

stationEndsL :: Lens' PlanetStation JulianDayTT
stationEndsL =
  simpleLens get set
  where
    get = stationEnds
    set s end' = s{stationEnds = end'}

stationTypeL :: Lens' PlanetStation Station
stationTypeL =
  simpleLens get set
  where
    get = stationType
    set s type' = s{stationType = type'}

stationPlanetL :: Lens' PlanetStation Planet
stationPlanetL =
  simpleLens get set
  where
    get = stationPlanet
    set s planet' = s{stationPlanet = planet'}


-------------------------------------------------------------------------------
-- Lenses for Event
-------------------------------------------------------------------------------

eventL :: Lens' ExactEvent Event
eventL =
  simpleLens get set
  where
    get = event
    set e evt' = e{event=evt'}

exactitudeL :: Lens' ExactEvent [UTCTime]
exactitudeL =
  simpleLens get set
  where
    get = exactitudeMoments
    set e exc' = e{exactitudeMoments = exc'}


-------------------------------------------------------------------------------
-- Traversals for Event
-------------------------------------------------------------------------------

_Event :: Traversal' ExactEvent Event
_Event f (ExactEvent e x) = ExactEvent <$> f e <*> pure x

_Exactitudes :: Traversal' ExactEvent [UTCTime]
_Exactitudes f (ExactEvent e xs) = ExactEvent e <$> f xs


_DirectionChangeInfo :: Traversal' Event PlanetStation
_DirectionChangeInfo f (DirectionChange info) = DirectionChange <$> f info
_DirectionChangeInfo _f evt = pure evt

{- 
NOTES:

* We could provide some lenses/traversals for queries, but I think @mundane@ and
  @natal@ do an okay job for now.
* By the same token, a Prism into @Query@ could prove useful, but, again, construction
  of queries isn't as painful as destruction of events.
-}
