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
import Data.Maybe (mapMaybe, fromJust)
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

mkUTC :: String -> UTCTime
mkUTC = fromJust . iso8601ParseM

spec :: Spec
spec = beforeAll_ epheWithFallback $ do
  describe "runQuery" $ do
    context "QueryDirectionChange" $ do
      it "finds all the changes of direction for Mercury in 2021" $ do
        let start2021 =  UTCTime (fromGregorian 2021 1 1) 0
            end2021   =  UTCTime (fromGregorian 2021 12 31) 0
            q = Mundane
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
        events <- runQuery q
        exactEvents <- eventsWithExactitude events
        let digest = extractStationInfo exactEvents
        toList digest `shouldBe` expectedStations
