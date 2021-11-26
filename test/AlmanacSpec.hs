{-# LANGUAGE OverloadedLists #-}
module AlmanacSpec (spec) where

import Test.Hspec
import System.Directory
import SwissEphemeris
import SwissEphemeris.Precalculated
import Almanac
import Data.Time
import Data.Foldable (toList)
import Data.Sequence
import Data.Function
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import Data.Bifunctor (second)
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

-- | Extract only the first moment of exactitude of retrograde or direct
-- stations.
extractStationInfo :: Seq ExactEvent -> [(Station, UTCTime)]
extractStationInfo evts =
  Data.Sequence.filter relevantStation evts
  & toList
  & mapMaybe summarize
  where
    relevantStation (ExactEvent (DirectionChange PlanetStation{stationType}) _) =
      stationType `elem` ([Direct, Retrograde] :: [Station])
    relevantStation _ = False
    summarize (ExactEvent (DirectionChange PlanetStation{stationType}) (firstExact:_)) =
      Just (stationType, firstExact)
    summarize _ = Nothing

extractCrossingInfo :: Seq ExactEvent -> [(Planet, PlanetMotion, ZodiacSignName, UTCTime)]
extractCrossingInfo evts =
  fmap summarize evts & toList & catMaybes
  where
    summarize (ExactEvent (ZodiacIngress (Crossing _s _e Zodiac{signName} planet motion)) (firstExact:_)) =
      Just (planet, motion, signName, firstExact)
    summarize _ = Nothing

extractMoonPhaseInfo :: Seq ExactEvent -> [(LunarPhaseName, UTCTime)]
extractMoonPhaseInfo evts =
  fmap summarize evts & toList & catMaybes
  where
    summarize (ExactEvent (LunarPhase LunarPhaseInfo{lunarPhaseName}) (firstExact:_)) =
      Just (lunarPhaseName, firstExact)
    summarize _ = Nothing
    
-- | Ugly function to "pretty print" events
genericEventInfo :: Seq ExactEvent ->  [(String, UTCTime)]
genericEventInfo evts =
  fmap summarize evts & toList & catMaybes
  where
    summarize (ExactEvent e (firstExact:_)) =
      case e of
        Eclipse info -> 
          case info of
            SolarEclipse eclType _ -> Just ("Solar Eclipse (" <> show eclType <> ")", firstExact)
            LunarEclipse eclType _ -> Just ("Lunar Eclipse (" <> show eclType <> ")", firstExact)
        DirectionChange PlanetStation{stationType, stationPlanet} ->
          if stationType `elem` ([Direct, Retrograde] :: [Station] ) then
            Just (show stationPlanet <> " goes " <> show stationType, firstExact)
          else
            Nothing
        ZodiacIngress (Crossing _s _e Zodiac{signName} planet motion) ->
          Just (show planet <> " enters " <> show signName <> " (" <> show motion <> ")", firstExact)
        LunarPhase LunarPhaseInfo{lunarPhaseName} ->
          if lunarPhaseName `elem` ([FullMoon, NewMoon] :: [LunarPhaseName]) then
            Just (show lunarPhaseName, firstExact)
          else
            Nothing
        _ -> Just ("not implemented yet", firstExact)
    summarize _ = Nothing

mkUTC :: String -> UTCTime
mkUTC = fromJust . iso8601ParseM

start2021, end2021 :: UTCTime
start2021 = UTCTime (fromGregorian 2021 1 1) 0
end2021 = UTCTime (fromGregorian 2022 1 1) 0

spec :: Spec
spec = beforeAll_ epheWithFallback $ do
  describe "runQuery" $ do
    context "QueryDirectionChange" $ do
      it "finds all the changes of direction for Mercury in 2021" $ do
        let q = Mundane
                  MundaneArgs{
                    mInterval = Interval start2021 end2021,
                    mQueries = [QueryDirectionChange [Mercury]]
                  }
            expectedStations =
              [
                (Retrograde,"2021-01-30T15:51:42.025379240512Z"),
                (Direct,"2021-02-21T00:51:59.892793893814Z"),
                (Retrograde,"2021-05-29T22:34:02.916525900363Z"),
                (Direct,"2021-06-22T22:00:02.674846351146Z"),
                (Retrograde,"2021-09-27T05:10:10.775578022003Z"),
                (Direct,"2021-10-18T15:16:50.452575981616Z")
              ] & map (second mkUTC)
        exactEvents <- runQuery q >>= eventsWithExactitude
        let digest = extractStationInfo exactEvents
        toList digest `shouldBe` expectedStations

    context "QueryZodiacIngress" $ do
      it "finds all ingresses in 2021" $ do
        let q = Mundane
                  MundaneArgs{
                    mInterval = Interval start2021 end2021,
                    mQueries = [QueryZodiacIngress [Mars, Jupiter, Saturn, Chiron, Uranus, Neptune, Pluto]]
                  }
            expectedCrossings =
              [
                (Mars,DirectMotion, Taurus,"2021-01-06T22:27:01.465341746807Z"),
                (Mars, DirectMotion,Gemini,"2021-03-04T03:29:34.011701345443Z"),
                (Mars, DirectMotion,Cancer,"2021-04-23T11:48:55.144302248954Z"),
                (Mars, DirectMotion,Leo,"2021-06-11T13:33:44.010010063648Z"),
                (Mars, DirectMotion,Virgo,"2021-07-29T20:32:32.573490142822Z"),
                (Mars, DirectMotion,Libra,"2021-09-15T00:13:56.056096851825Z"),
                (Mars, DirectMotion,Scorpio,"2021-10-30T14:21:06.681294143199Z"),
                (Mars, DirectMotion,Sagittarius,"2021-12-13T09:52:56.338474452495Z"),
                (Jupiter, DirectMotion, Pisces,"2021-05-13T22:36:03.055363297462Z"),
                (Jupiter, RetrogradeMotion, Aquarius,"2021-07-28T12:42:09.763747751712Z"),
                (Jupiter, DirectMotion, Pisces,"2021-12-29T04:09:37.511599659919Z")
              ] & map (second mkUTC)
        exactEvents <- runQuery q >>= eventsWithExactitude
        let digest = extractCrossingInfo exactEvents
        digest `shouldBe` expectedCrossings

    context "QueryLunarPhase" $ do
      it "finds all lunar phases in November 2021" $ do
        let nov2021 = UTCTime (fromGregorian 2021 11 1) 0
            dec2021 = UTCTime (fromGregorian 2021 12 1) 0
            q = Mundane
                  MundaneArgs{
                    mInterval = Interval nov2021 dec2021,
                    mQueries = [QueryLunarPhase]
                  }
            expectedPhases = 
              [
                (WaningCrescent,"2021-11-01T13:28:27.314121723175Z"),
                (NewMoon,"2021-11-04T21:14:36.684200763702Z"),
                (WaxingCrescent,"2021-11-08T02:31:28.868464529514Z"),
                (FirstQuarter,"2021-11-11T12:46:02.566146254539Z"),
                (WaxingGibbous,"2021-11-15T07:27:48.519482016563Z"),
                (FullMoon,"2021-11-19T08:57:27.984892129898Z"),
                (WaningGibbous,"2021-11-23T12:50:27.58442312479Z"),
                (LastQuarter,"2021-11-27T12:27:40.648325085639Z")
              ] & map (second mkUTC)
        exactPhases <- runQuery q >>= eventsWithExactitude
        let digest = extractMoonPhaseInfo exactPhases
        digest `shouldBe` expectedPhases

    context "QueryEclipse" $ do
      it "finds all eclipses for 2021" $ do
        let q = Mundane
                  MundaneArgs{
                    mInterval = Interval start2021 end2021,
                    mQueries = [QueryEclipse]
                  }
            expectedEclipses =
              [
                ("Solar Eclipse (AnnularEclipse)","2021-06-10T10:41:56.099877655506Z"),
                ("Solar Eclipse (TotalSolarEclipse)","2021-12-04T07:33:28.516712486743Z"),
                ("Lunar Eclipse (TotalLunarEclipse)","2021-05-26T11:18:43.071934282779Z"),
                ("Lunar Eclipse (PartialLunarEclipse)","2021-11-19T09:02:55.849740207195Z")
              ] & map (second mkUTC)
        exactEcl <- runQuery q >>= eventsWithExactitude
        let digest = genericEventInfo exactEcl
        digest `shouldBe` expectedEclipses

    context "Composite mundane query" $ do
      it "finds all events for October/November 2021" $ do
        let oct2021 = UTCTime (fromGregorian 2021 10 1) 0
            dec2021 = UTCTime (fromGregorian 2021 12 1) 0
            q = Mundane
                  MundaneArgs{
                    mInterval = Interval oct2021 dec2021,
                    mQueries = [
                      QueryDirectionChange [Mercury],
                      QueryZodiacIngress [Mars, Jupiter, Saturn, Chiron, Uranus, Neptune, Pluto],
                      QueryEclipse,
                      QueryLunarPhase
                    ]
                  }
            expectedEvents = 
              [
                ("Mercury goes Direct","2021-10-18T15:16:50.452575981616Z"),
                ("Mars enters Scorpio (DirectMotion)","2021-10-30T14:21:06.681294143199Z"),
                ("NewMoon","2021-10-06T11:05:24.023683369159Z"),
                ("FullMoon","2021-10-20T14:56:41.52483433485Z"),
                ("NewMoon","2021-11-04T21:14:36.684200763702Z"),
                ("FullMoon","2021-11-19T08:57:27.984892129898Z"),
                ("Lunar Eclipse (PartialLunarEclipse)","2021-11-19T09:02:55.849740207195Z")
              ] & map (second mkUTC)
        exactEvents <- runQuery q >>= eventsWithExactitude 
        let digest = genericEventInfo exactEvents
        digest `shouldBe` expectedEvents

 