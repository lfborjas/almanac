module Almanac.Event.Eclipse where

import SwissEphemeris
    ( nextLunarEclipseWhen,
      nextSolarEclipseWhen,
      EventSearchDirection(SearchForward),
      LunarEclipseType,
      SolarEclipseType,
      JulianDay(getJulianDay),
      JulianDayUT1 )
import Almanac.Event.Types ( EclipseInfo(..) )

getEclipseDate :: EclipseInfo -> JulianDayUT1
getEclipseDate (SolarEclipse _ d) = d
getEclipseDate (LunarEclipse _ d) = d

allEclipses :: JulianDayUT1 -> JulianDayUT1 -> IO [EclipseInfo]
allEclipses start end =
  solarEclipses start end <> lunarEclipses start end

solarEclipses :: JulianDayUT1 -> JulianDayUT1 -> IO [EclipseInfo]
solarEclipses = unfoldEclipses anySolarEclipse SolarEclipse 
 
lunarEclipses :: JulianDayUT1 -> JulianDayUT1 -> IO [EclipseInfo]
lunarEclipses = unfoldEclipses anyLunarEclipse LunarEclipse

unfoldEclipses 
  :: (JulianDayUT1 -> IO (Either String (a, JulianDayUT1)))
  -> (a -> JulianDayUT1 -> EclipseInfo)
  -> JulianDayUT1 
  -> JulianDayUT1 
  -> IO [EclipseInfo]
unfoldEclipses findEclipse mkEclipse start end = do
  nextEclipse <- findEclipse start
  case nextEclipse of
    Left _ -> return []
    Right (eclType, eclMax) -> do
      if getJulianDay eclMax >= getJulianDay end then
        return []
      else do
        others <- unfoldEclipses findEclipse mkEclipse eclMax end
        let eclInfo = mkEclipse eclType eclMax
        return (eclInfo : others)
 
anySolarEclipse :: JulianDayUT1 -> IO (Either String (SolarEclipseType, JulianDayUT1))
anySolarEclipse = nextSolarEclipseWhen [] SearchForward

anyLunarEclipse :: JulianDayUT1 -> IO (Either String (LunarEclipseType, JulianDayUT1))
anyLunarEclipse = nextLunarEclipseWhen [] SearchForward
