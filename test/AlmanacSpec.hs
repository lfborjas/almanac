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
