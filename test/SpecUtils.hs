module SpecUtils where

import Almanac
import System.Directory
import SwissEphemeris
import SwissEphemeris.Precalculated
import Data.Time
import Data.Foldable (toList)
import Data.Sequence hiding (zip, fromList)
import Data.Function
import Data.Maybe (fromJust, catMaybes)
import Data.Time.Format.ISO8601 (iso8601ParseM)


ephePath :: FilePath
ephePath = "./test/ephe/"

epheWithFallback :: IO ()
epheWithFallback = do
  fullEphePath <- makeAbsolute ephePath
  -- location of ephe data
  setEphemeridesPath fullEphePath
  -- location of precalculated ephe
  setEphe4Path fullEphePath

-- | Ugly function to "pretty print" events
genericEventInfo :: Seq ExactEvent ->  [(String, [UTCTime])]
genericEventInfo evts =
  fmap summarize evts & toList & catMaybes
  where
    summarize (ExactEvent e exacts) =
      case e of
        Eclipse info -> 
          case info of
            SolarEclipse eclType _ -> Just ("Solar Eclipse (" <> show eclType <> ")", exacts)
            LunarEclipse eclType _ -> Just ("Lunar Eclipse (" <> show eclType <> ")", exacts)
        DirectionChange PlanetStation{stationType, stationPlanet} ->
          if stationType `elem` ([Direct, Retrograde] :: [Station] ) then
            Just (show stationPlanet <> " goes " <> show stationType, exacts)
          else
            Nothing
        ZodiacIngress (Crossing _s _e Zodiac{signName} planet motion) ->
          Just (show planet <> " enters " <> show signName <> " (" <> show motion <> ")", exacts)
        HouseIngress  (Crossing _s _e House{houseName} planet motion) ->
          Just (show planet <> " enters house " <> show houseName <> " (" <> show motion <> ")", exacts)
        LunarPhase LunarPhaseInfo{lunarPhaseName} ->
          if lunarPhaseName `elem` ([FullMoon, NewMoon] :: [LunarPhaseName]) then
            Just (show lunarPhaseName, exacts)
          else
            Nothing
        PlanetaryTransit Transit{transiting, aspect, transited} ->
          Just (show transiting <> " " <> show aspect <> " " <> show transited, exacts)
        HouseTransit Transit{transiting, aspect, transited} ->
          Just (show transiting <> " " <> show aspect <> " " <> (show . houseName $ transited), exacts)

mkUTC :: String -> UTCTime
mkUTC = fromJust . iso8601ParseM
