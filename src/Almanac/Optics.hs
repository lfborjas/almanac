{- | 
This module provides optics into queries and events. @Control.Lens@-compatible
lenses and traversals are provided (prisms would also be possible,
but that'd introduce a dependency on @profunctors@).

In keeping with less magical lens approaches (e.g. Relude or RIO,) instead
of lenses having the more obvious name and types having to carry an underscore
prefix, we suffix lenses here with @L@. Traversals do follow the less
contentious tradition from @Control.Lens@ and are prefixed with underscore.
-}

module Almanac.Optics where

import SwissEphemeris
import Almanac.Internal.Lens
import Almanac.Event.Types
import Data.Time (UTCTime)
import Almanac.EclipticLongitude
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
-- Lenses for 'Crossing', 'Zodiac' and 'House'
-------------------------------------------------------------------------------

crossingStartsL :: Lens' (Crossing a) JulianDayTT
crossingStartsL =
  simpleLens get set
  where
    get = crossingStarts
    set c start' = c{crossingStarts = start'}

crossingEndsL :: Lens' (Crossing a) JulianDayTT
crossingEndsL =
  simpleLens get set
  where
    get = crossingEnds
    set c end' = c{crossingEnds = end'}

crossingCrossesL :: Lens' (Crossing a) a
crossingCrossesL =
  simpleLens get set
  where
    get = crossingCrosses
    set c x' = c{crossingCrosses = x'}

crossingPlanetL :: Lens' (Crossing a) Planet
crossingPlanetL =
  simpleLens get set
  where
    get = crossingPlanet
    set c p' = c{crossingPlanet = p'}

crossingDirectionL :: Lens' (Crossing a) PlanetMotion
crossingDirectionL =
  simpleLens get set
  where
    get = crossingDirection
    set c d' = c{crossingDirection = d'}

signNameL :: Lens' Zodiac ZodiacSignName
signNameL =
  simpleLens get set
  where
    get = signName
    set z n' = z{signName = n'}

signStartL :: Lens' Zodiac Double
signStartL =
  simpleLens get set
  where
    get = signStart
    set z s' = z{signStart = s'}

signEndL :: Lens' Zodiac Double
signEndL =
  simpleLens get set
  where
    get = signEnd
    set z e' = z{signEnd = e'}

houseNameL :: Lens' House HouseName
houseNameL =
  simpleLens get set
  where
    get = houseName
    set z n' = z{houseName = n'}

houseCuspL :: Lens' House Double
houseCuspL =
  simpleLens get set
  where
    get = houseCusp
    set z s' = z{houseCusp = s'}

houseEndL :: Lens' House Double
houseEndL =
  simpleLens get set
  where
    get = houseEnd
    set z e' = z{houseEnd = e'}

-------------------------------------------------------------------------------
-- Traversals for 'Crossing'
-------------------------------------------------------------------------------

_CrossedZodiac :: Traversal' (Crossing Zodiac) Zodiac
_CrossedZodiac = traverseCrossing

_CrossedHouse :: Traversal' (Crossing House) House
_CrossedHouse = traverseCrossing

traverseCrossing :: Traversal' (Crossing a) a
traverseCrossing f (Crossing start end crosses planet direction)
  = Crossing start end <$> f crosses <*> pure planet <*> pure direction
  
 
-------------------------------------------------------------------------------
-- Lenses for Transit
-------------------------------------------------------------------------------

aspectL :: Lens' (Transit a) AspectName
aspectL =
  simpleLens get set
  where
    get = aspect
    set t a' = t{aspect = a'} 
    
transitingL :: Lens' (Transit a) Planet
transitingL =
  simpleLens get set
  where
    get = transiting
    set t p'= t{transiting = p'}
    
transitedL :: Lens' (Transit over) over
transitedL =
  simpleLens get set
  where
    get = transited
    set t t' = t{transited = t'}
    
transitAngleL :: Lens' (Transit a) Double
transitAngleL =
  simpleLens get set
  where
    get = transitAngle
    set t a' = t{transitAngle = a'}

transitOrbL :: Lens' (Transit a) Double
transitOrbL =
  simpleLens get set
  where
    get = transitOrb
    set t o' = t{transitOrb = o'}

transitStartsL :: Lens' (Transit a) JulianDayTT
transitStartsL =
  simpleLens get set
  where
    get = transitStarts
    set t s' = t{transitStarts = s'}

transitEndsL :: Lens' (Transit a) JulianDayTT
transitEndsL =
  simpleLens get set
  where
    get = transitEnds
    set t e' = t{transitEnds = e'}

transitPhasesL :: Lens' (Transit a) (MergeSeq TransitPhase)
transitPhasesL =
  simpleLens get set
  where
    get = transitPhases
    set t p' = t{transitPhases = p'}
    
transitIsExactL :: Lens' (Transit a) [JulianDayTT]
transitIsExactL =
  simpleLens get set
  where
    get = transitIsExact
    set t e' = t{transitIsExact = e'}
    
transitCrossesL :: Lens' (Transit a) EclipticLongitude
transitCrossesL =
  simpleLens get set
  where
    get = transitCrosses
    set t c' = t{transitCrosses = c'}
    
phaseNameL :: Lens' TransitPhase TransitPhaseName 
phaseNameL =
  simpleLens get set
  where
    get = phaseName
    set p n' = p{phaseName = n'} 
    
phaseStartsL :: Lens' TransitPhase JulianDayTT
phaseStartsL =
  simpleLens get set
  where
    get = phaseStarts 
    set p at' = p{phaseStarts = at'} 

phaseEndsL :: Lens' TransitPhase JulianDayTT
phaseEndsL =
  simpleLens get set
  where
    get = phaseEnds 
    set p at' = p{phaseEnds = at'} 

-------------------------------------------------------------------------------
-- Traversals for 'Transit'
-------------------------------------------------------------------------------

_TransitedPlanet :: Traversal' (Transit Planet) Planet
_TransitedPlanet = traverseTransit

_TransitedHouse :: Traversal' (Transit House) House
_TransitedHouse = traverseTransit

traverseTransit :: Traversal' (Transit over) over
traverseTransit f (Transit aspectName transits over angle orb starts ends phases exact crossesLng) =
  Transit aspectName transits <$> f over 
                              <*> pure angle 
                              <*> pure orb 
                              <*> pure starts 
                              <*> pure ends 
                              <*> pure phases 
                              <*> pure exact 
                              <*> pure crossesLng

-------------------------------------------------------------------------------
-- Lenses for LunarPhaseInfo
-------------------------------------------------------------------------------

lunarPhaseNameL :: Lens' LunarPhaseInfo LunarPhaseName 
lunarPhaseNameL =
  simpleLens get set
  where
    get = lunarPhaseName
    set l n' = l{lunarPhaseName = n'}
    
lunarPhaseStartsL :: Lens' LunarPhaseInfo JulianDayTT
lunarPhaseStartsL =
  simpleLens get set
  where
    get = lunarPhaseStarts
    set l at' = l{lunarPhaseStarts = at'}

lunarPhaseEndsL :: Lens' LunarPhaseInfo JulianDayTT
lunarPhaseEndsL =
  simpleLens get set
  where
    get = lunarPhaseEnds
    set l at' = l{lunarPhaseEnds = at'}

-------------------------------------------------------------------------------
-- Traversals for EclipseInfo
-------------------------------------------------------------------------------

_SolarEclipseType :: Traversal' EclipseInfo SolarEclipseType
_SolarEclipseType f (SolarEclipse typ exactAt) =
  SolarEclipse <$> f typ <*> pure exactAt
_SolarEclipseType _f otherEcl = pure otherEcl

_LunarEclipseType :: Traversal' EclipseInfo LunarEclipseType
_LunarEclipseType f (LunarEclipse typ exactAt) =
  LunarEclipse <$> f typ <*> pure exactAt
_LunarEclipseType _f otherEcl = pure otherEcl

_EclipseExactAt :: Traversal' EclipseInfo JulianDayUT1 
_EclipseExactAt f (SolarEclipse t exactAt) = 
  SolarEclipse t <$> f exactAt
_EclipseExactAt f (LunarEclipse t exactAt) = 
  LunarEclipse t <$> f exactAt

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
-- Traversals for Event and ancillary types
-------------------------------------------------------------------------------

_Event :: Traversal' ExactEvent Event
_Event f (ExactEvent e x) = ExactEvent <$> f e <*> pure x

_Exactitudes :: Traversal' ExactEvent [UTCTime]
_Exactitudes f (ExactEvent e xs) = ExactEvent e <$> f xs


_DirectionChangeInfo :: Traversal' Event PlanetStation
_DirectionChangeInfo f (DirectionChange info) = DirectionChange <$> f info
_DirectionChangeInfo _ evt = pure evt

_ZodiacIngressInfo :: Traversal' Event (Crossing Zodiac)
_ZodiacIngressInfo f (ZodiacIngress info) = ZodiacIngress <$> f info
_ZodiacIngressInfo _ evt = pure evt

_HouseIngressInfo :: Traversal' Event (Crossing House)
_HouseIngressInfo f (HouseIngress info) = HouseIngress <$> f info
_HouseIngressInfo _ evt = pure evt

_PlanetaryTransitInfo :: Traversal' Event (Transit Planet)
_PlanetaryTransitInfo f (PlanetaryTransit info) = PlanetaryTransit  <$> f info
_PlanetaryTransitInfo _ evt = pure evt

_HouseTransitInfo :: Traversal' Event (Transit House)
_HouseTransitInfo f (HouseTransit info) = HouseTransit <$> f info
_HouseTransitInfo _ evt = pure evt

_LunarPhaseInfo :: Traversal' Event LunarPhaseInfo
_LunarPhaseInfo f (LunarPhase info) = LunarPhase <$> f info
_LunarPhaseInfo _ evt = pure evt

_EclipseInfo :: Traversal' Event EclipseInfo 
_EclipseInfo f (Eclipse info) = Eclipse <$> f info
_EclipseInfo _ evt = pure evt

{- 
NOTES:

* We could provide some lenses/traversals for queries, but I think @mundane@ and
  @natal@ do an okay job for now.
* By the same token, a Prism into @Query@ could prove useful, but, again, construction
  of queries isn't as painful as deconstruction of events.
-}
