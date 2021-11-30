{-# LANGUAGE OverloadedLists #-}
module AlmanacSpec (spec) where

import Test.Hspec
import Almanac
import Almanac.Extras
import Almanac.Optics
    ( stationTypeL,
      _DirectionChangeInfo,
      eventL,
      exactitudeMomentsL,
      crossingCrossesL,
      crossingPlanetL,
      crossingDirectionL,
      signNameL,
      lunarPhaseNameL,
      _ZodiacIngressInfo,
      _LunarPhaseInfo, eventL)
import Lens.Micro
    ( (&), (^..), (^?), _Just, _head, filtered, traversed )

import SwissEphemeris
    ( GeographicPosition(GeographicPosition, geoLat, geoLng),
      LunarPhaseName(LastQuarter, WaningCrescent, NewMoon,
                     WaxingCrescent, FirstQuarter, WaxingGibbous, FullMoon,
                     WaningGibbous),
      ZodiacSignName(Pisces, Taurus, Gemini, Cancer, Leo, Virgo, Libra,
                     Scorpio, Sagittarius, Aquarius),
      PlanetMotion(DirectMotion, RetrogradeMotion),
      Planet(Moon, Mercury, Mars, Jupiter, Saturn, Chiron, Uranus,
             Neptune, Pluto) )
import Data.Time ( fromGregorian, UTCTime(UTCTime) )
import Data.Foldable (toList)
import Data.Bifunctor (second)
import Data.List.NonEmpty (fromList)
import qualified Data.Sequence as S
import SpecUtils
    ( epheWithFallback,
      genericEventInfo,
      mkUTC )
import Lens.Micro.Extras (view)

start2021, end2021 :: UTCTime
start2021 = UTCTime (fromGregorian 2021 1 1) 0
end2021 = UTCTime (fromGregorian 2022 1 1) 0

-- Results verified at: https://cafeastrology.com/astrology-of-2021.html
spec :: Spec
spec = beforeAll_ epheWithFallback $ do
  describe "runQuery" $ do
    context "QueryDirectionChange" $ do
      it "finds all the changes of direction for Mercury in 2021" $ do
        let q = mundane
                  (Interval start2021 end2021)
                  [QueryDirectionChange [Mercury]]
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
        let digest = (summarize <$> exactEvents) ^.. traversed . _Just
            summarize evt =
              let stationT  = evt ^? eventL._DirectionChangeInfo.stationTypeL.filtered isChange
                  isChange = (`elem` ([Direct, Retrograde] :: [Station]))
                  firstExact = evt ^? exactitudeMomentsL._head
              in  (,) <$> stationT <*> firstExact
            
        toList digest `shouldBe` expectedStations

    context "QueryZodiacIngress" $ do
      it "finds all ingresses in 2021" $ do
        let q = mundane
                  (Interval start2021 end2021)
                  [QueryZodiacIngress [Mars, Jupiter, Saturn, Chiron, Uranus, Neptune, Pluto]]
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
        let digest = (summarize <$> exactEvents) ^.. traversed . _Just
            summarize evt =
              let ingress    = evt ^? eventL._ZodiacIngressInfo
                  firstExact = evt ^? exactitudeMomentsL._head
                  ingressingPlanet =
                    view crossingPlanetL    <$> ingress
                  ingressingMotion =
                    view crossingDirectionL <$> ingress
                  ingressed =
                    (view (crossingCrossesL.signNameL) <$> ingress)
              in
                (,,,) <$> ingressingPlanet
                      <*> ingressingMotion
                      <*> ingressed
                      <*> firstExact
        digest `shouldBe` expectedCrossings

    context "QueryLunarPhase" $ do
      it "finds all lunar phases in November 2021" $ do
        let nov2021 = UTCTime (fromGregorian 2021 11 1) 0
            dec2021 = UTCTime (fromGregorian 2021 12 1) 0
            q = mundane
                  (Interval nov2021 dec2021)
                  [QueryLunarPhase]
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
        let digest = (summarize <$> exactPhases) ^.. traversed . _Just
            summarize evt =
              let phase      = evt ^? eventL._LunarPhaseInfo.lunarPhaseNameL
                  firstExact = evt ^? exactitudeMomentsL._head
              in (,) <$> phase <*> firstExact
        digest `shouldBe` expectedPhases

    context "QueryEclipse" $ do
      it "finds all eclipses for 2021" $ do
        let q = mundane
                  (Interval start2021 end2021)
                  [QueryEclipse]
            expectedEclipses =
              [
                ("Solar Eclipse (AnnularEclipse)","2021-06-10T10:41:56.099877655506Z"),
                ("Solar Eclipse (TotalSolarEclipse)","2021-12-04T07:33:28.516712486743Z"),
                ("Lunar Eclipse (TotalLunarEclipse)","2021-05-26T11:18:43.071934282779Z"),
                ("Lunar Eclipse (PartialLunarEclipse)","2021-11-19T09:02:55.849740207195Z")
              ] & map (second (pure . mkUTC))
        exactEcl <- runQuery q >>= eventsWithExactitude
        let digest = genericEventInfo exactEcl
        digest `shouldBe` expectedEclipses

    context "Composite mundane query" $ do
      it "finds all events for October/November 2021" $ do
        let oct2021 = UTCTime (fromGregorian 2021 10 1) 0
            dec2021 = UTCTime (fromGregorian 2021 12 1) 0
            q = mundane
                  (Interval oct2021 dec2021)
                  [
                    QueryDirectionChange [Mercury],
                    QueryZodiacIngress [Mars, Jupiter, Saturn, Chiron, Uranus, Neptune, Pluto],
                    QueryEclipse,
                    QueryLunarPhase
                  ]
            expectedEvents =
              [
                ("Mercury goes Direct","2021-10-18T15:16:50.452575981616Z"),
                ("Mars enters Scorpio (DirectMotion)","2021-10-30T14:21:06.681294143199Z"),
                ("NewMoon","2021-10-06T11:05:24.023683369159Z"),
                ("FullMoon","2021-10-20T14:56:41.52483433485Z"),
                ("NewMoon","2021-11-04T21:14:36.684200763702Z"),
                ("FullMoon","2021-11-19T08:57:27.984892129898Z"),
                ("Lunar Eclipse (PartialLunarEclipse)","2021-11-19T09:02:55.849740207195Z")
              ] & map (second (pure . mkUTC))
        exactEvents <- runQuery q >>= eventsWithExactitude
        let digest = genericEventInfo exactEvents
        digest `shouldBe` expectedEvents

    context "Composite natal query" $ do
      it "finds all events for an interval, for a reference event" $ do
        let gestern = UTCTime (fromGregorian 2021 11 25) 0
            morgen  = UTCTime (fromGregorian 2021 11 27) 0
            q = natal
                  (Interval gestern morgen)
                  (ReferenceEvent
                    (mkUTC "1989-01-07T05:30:00Z")
                    (GeographicPosition {geoLat = 14.0839053, geoLng = -87.2750137} ))
                  [
                    QueryHouseIngress [Moon],
                    -- all possible pairs, without a transiting Moon
                    QueryPlanetaryNatalTransit (fromList majorAspects) (fromList defaultNatalTransitPairs),
                    QueryCuspTransit (fromList majorAspects) (fromList defaultCuspTransitPairs),
                    QueryLunarNatalTransit (fromList majorAspects) (fromList defaultPlanets),
                    QueryLunarCuspTransit (fromList majorAspects) (fromList defaultHouses)
                  ]
            expectedEvents =
              [
                ("Moon enters house XI (DirectMotion)","2021-11-25T13:44:39.7735825181Z"),
                ("Sun Quincunx Chiron","2021-11-25T12:42:25.910954475402Z"),
                ("Mercury SemiSextile Uranus","2021-11-25T23:51:01.098799109458Z"),
                ("Mercury Quincunx Chiron","2021-11-26T20:08:33.325251638889Z"),
                ("Venus Conjunction Sun","2021-11-25T11:33:25.524118244647Z"),
                ("Moon Quincunx Sun","2021-11-26T01:08:47.985133230686Z"),
                ("Moon Quincunx Moon","2021-11-25T10:29:47.240101397037Z"),
                ("Moon Opposition Mercury","2021-11-25T03:30:50.045527517795Z"),
                ("Moon Trine Venus","2021-11-26T17:49:45.178953409194Z"),
                ("Moon Trine Mars","2021-11-26T13:42:52.33622521162Z"),
                ("Moon Square Jupiter","2021-11-26T19:20:45.90741097927Z"),
                ("Moon Quincunx Saturn","2021-11-25T04:29:24.774884283542Z"),
                ("Moon Quincunx Neptune","2021-11-25T12:00:54.584334790706Z"),
                ("Moon Square Pluto","2021-11-25T20:53:55.02881526947Z"),
                ("Moon Quincunx MeanNode","2021-11-25T06:43:07.285946309566Z"),
                ("Moon SemiSextile MeanApog","2021-11-26T19:24:13.485766053199Z"),
                ("Moon SemiSextile X","2021-11-25T13:42:39.953512251377Z"),
                ("Moon Sextile I","2021-11-25T15:09:36.979006826877Z")
              ] & map (second (pure . mkUTC))
        exactEvents <- runQuery q >>= eventsWithExactitude
        let digest = genericEventInfo $ S.filter hasExactitude exactEvents
        digest `shouldBe` expectedEvents
        
      it "finds a specific transit for a longer interval" $ do
        let weekStart = UTCTime (fromGregorian 2021 11 28) 0
            weekEnd   = UTCTime (fromGregorian 2021 12 5) 0
            q = natal
                  (Interval weekStart weekEnd)
                  (ReferenceEvent
                    (mkUTC "1989-01-07T05:30:00Z")
                    (GeographicPosition {geoLat = 14.0839053, geoLng = -87.2750137} ))
                  [
                    -- relax the orb for sesquisquare from its very stingy default
                    QueryPlanetaryNatalTransit [sesquisquare {orbApplying = 3}] [(Mercury, Mars)]
                  ]
            expectedEvents =
              [
                ("Mercury Sesquisquare Mars","2021-11-29T23:55:56.071771681308Z")
              ] & map (second (pure . mkUTC))
        exactEvents <- runQuery q >>= eventsWithExactitude
        let digest = genericEventInfo $ S.filter hasExactitude exactEvents
        digest `shouldBe` expectedEvents
