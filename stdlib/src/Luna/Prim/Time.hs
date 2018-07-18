{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.Time where

import Prologue

import qualified Luna.IR as IR

import qualified Data.Map                    as Map
import qualified Data.Time                   as Time
import qualified Data.Time.Calendar          as Time
import qualified Data.Time.Format            as Time
import qualified Luna.Pass.Sourcing.Data.Def as Def
import qualified Luna.Runtime                as Luna
import qualified Luna.Std.Builder            as Builder
import qualified OCI.Data.Name               as Name

import           Data.Fixed                  (Pico)
import           Data.Map                    (Map)
import           Data.Time                   (DiffTime)
import           Luna.Std.Builder            ( makeFunctionIO
                                             , makeFunctionPure
                                             , LTp (..), int, integer, real
                                             )

type TimeModule = "Std.Time"

timeModule :: Name.Qualified
timeModule = Name.qualFromSymbol @TimeModule

timeLT, utcTimeLT, timeZoneLT, timeIntervalLT :: LTp
timeLT         = LCons timeModule "Time" []
utcTimeLT      = LCons timeModule "UTCTime" []
timeZoneLT     = LCons timeModule "TimeZone" []
timeIntervalLT = LCons timeModule "TimeInterval" []


pico :: Double -> Pico
pico = realToFrac

exports :: forall graph m.
    ( Builder.StdBuilder graph m
    ) => m (Map IR.Name Def.Def)
exports = do
    let intT    = Builder.intLT
        tuple3T = Builder.tuple3LT

    let primGetCurrentTimeVal :: IO Time.ZonedTime
        primGetCurrentTimeVal = Time.getZonedTime
    primGetCurrentTime <- makeFunctionIO @graph (flip Luna.toValue primGetCurrentTimeVal) [] timeLT

    let primGetCurrentTimeZoneVal :: IO Time.TimeZone
        primGetCurrentTimeZoneVal = Time.getCurrentTimeZone
    primGetCurrentTimeZone <- makeFunctionIO @graph (flip Luna.toValue primGetCurrentTimeZoneVal) [] timeZoneLT

    let primTimeToUTCVal = flip Luna.toValue Time.zonedTimeToUTC
    primTimeToUTC <- makeFunctionPure @graph primTimeToUTCVal [timeLT] utcTimeLT

    let primTimeFromUTCVal = flip Luna.toValue Time.utcToZonedTime
    primTimeFromUTC <- makeFunctionPure @graph primTimeFromUTCVal [timeZoneLT, utcTimeLT] timeLT

    let primDiffTimesVal = flip Luna.toValue Time.diffUTCTime
    primDiffTimes <- makeFunctionPure @graph primDiffTimesVal [utcTimeLT, utcTimeLT] timeIntervalLT

    let fmtTime :: Text -> Time.ZonedTime -> Text
        fmtTime fmt = convert . Time.formatTime Time.defaultTimeLocale (convert fmt :: String)
    primFormatTime <- makeFunctionPure @graph (flip Luna.toValue fmtTime) [Builder.textLT, timeLT] Builder.textLT

    let fmtUTCTime :: Text -> Time.UTCTime -> Text
        fmtUTCTime fmt = convert . Time.formatTime Time.defaultTimeLocale (convert fmt :: String)
    primFormatUTCTime <- makeFunctionPure @graph (flip Luna.toValue fmtUTCTime) [Builder.textLT, utcTimeLT] Builder.textLT

    let primTimesEqVal = flip Luna.toValue ((==) :: Time.UTCTime -> Time.UTCTime -> Bool)
    primTimesEq <- makeFunctionPure @graph primTimesEqVal [utcTimeLT, utcTimeLT] Builder.boolLT

    let primAddUTCTimeVal = flip Luna.toValue Time.addUTCTime
        primSubUTCTimeVal = flip Luna.toValue (\d t -> Time.addUTCTime (-d) t)
    primAddUTCTime <- makeFunctionPure @graph primAddUTCTimeVal [timeIntervalLT, utcTimeLT] utcTimeLT
    primSubUTCTime <- makeFunctionPure @graph primSubUTCTimeVal [timeIntervalLT, utcTimeLT] utcTimeLT

    let primTimeOfDayVal :: Time.DiffTime -> (Integer, Integer, Double)
        primTimeOfDayVal t = let Time.TimeOfDay h m s = Time.timeToTimeOfDay t in (convert h, convert m, realToFrac s)
    primTimeOfDay <- makeFunctionPure @graph (flip Luna.toValue primTimeOfDayVal) [timeIntervalLT] $ tuple3T intT intT intT

    let primTimeOfYearVal :: Time.Day -> (Integer, Integer, Integer)
        primTimeOfYearVal t = let (y, m, d) = Time.toGregorian t in (y, integer m, integer d)
    primTimeOfYear <- makeFunctionPure @graph (flip Luna.toValue primTimeOfYearVal) [intT] $ tuple3T intT intT intT

    let primFromTimeOfYearVal :: Integer -> Integer -> Integer -> Time.Day
        primFromTimeOfYearVal y m d = Time.fromGregorian y (int m) (int d)
    primFromTimeOfYear <- makeFunctionPure @graph (flip Luna.toValue primFromTimeOfYearVal) [intT, intT, intT] intT

    let primMonthLengthVal :: Integer -> Integer -> Integer
        primMonthLengthVal y m = integer $ Time.gregorianMonthLength y (int m)
    primMonthLength <- makeFunctionPure @graph (flip Luna.toValue primMonthLengthVal) [intT, intT] intT

    let parseTime :: Text -> Text -> Maybe Time.ZonedTime
        parseTime fmt str = Time.parseTimeM True Time.defaultTimeLocale (convert fmt) (convert str)
    primParseTime <- makeFunctionPure @graph (flip Luna.toValue parseTime) [Builder.textLT, Builder.textLT] (Builder.maybeLT timeLT)

    let primIntMilisecondsVal :: Integer -> Time.DiffTime
        primIntMilisecondsVal = (/ 1000) . realToFrac
    primIntMiliseconds <- makeFunctionPure @graph (flip Luna.toValue primIntMilisecondsVal) [Builder.intLT] timeIntervalLT

    return $ Map.fromList [ ("primGetCurrentTime", primGetCurrentTime)
                          , ("primGetCurrentTimeZone", primGetCurrentTimeZone)
                          , ("primTimeToUTC", primTimeToUTC)
                          , ("primTimeFromUTC", primTimeFromUTC)
                          , ("primDiffTimes", primDiffTimes)
                          , ("primFormatTime", primFormatTime)
                          , ("primFormatUTCTime", primFormatUTCTime)
                          , ("primTimesEq", primTimesEq)
                          , ("primAddUTCTime", primAddUTCTime)
                          , ("primSubUTCTime", primSubUTCTime)
                          , ("primTimeOfDay", primTimeOfDay)
                          , ("primTimeOfYear", primTimeOfYear)
                          , ("primFromTimeOfYear", primFromTimeOfYear)
                          , ("primMonthLength", primMonthLength)
                          , ("primParseTime", primParseTime)
                          , ("primIntMiliseconds", primIntMiliseconds)
                          ]


type instance Luna.RuntimeRepOf Time.DiffTime =
    Luna.AsClass Time.DiffTime ('Luna.ClassRep TimeModule "TimeInterval")
type instance Luna.RuntimeRepOf Time.UTCTime =
    Luna.AsClass Time.UTCTime ('Luna.ClassRep TimeModule "UTCTime")
type instance Luna.RuntimeRepOf Time.TimeOfDay =
    Luna.AsClass Time.TimeOfDay ('Luna.ClassRep TimeModule "TimeOfDay")
type instance Luna.RuntimeRepOf Time.TimeZone =
    Luna.AsClass Time.TimeZone ('Luna.ClassRep TimeModule "TimeZone")
type instance Luna.RuntimeRepOf Time.ZonedTime =
    Luna.AsClass Time.ZonedTime ('Luna.ClassRep TimeModule "Time")


instance Luna.FromData Time.NominalDiffTime where
    fromData dt = realToFrac <$> (Luna.fromData dt :: Luna.Eff Time.DiffTime)

instance Luna.FromData Time.Day where
    fromData d = Time.ModifiedJulianDay <$> Luna.fromData d


instance Luna.FromObject Time.DiffTime where
    fromConstructor = \case
        Luna.Constructor "TimeInterval" [f] ->
            fmap Time.picosecondsToDiffTime . Luna.fromData $ f
        _ -> Luna.throw
            "Expected a TimeInterval luna object, got unexpected constructor"

instance Luna.FromObject Time.UTCTime where
    fromConstructor = \case
        Luna.Constructor "UTCTimeVal" [days, diff] ->
            Time.UTCTime <$> Luna.fromData days
                         <*> Luna.fromData diff
        _ -> Luna.throw
            "Expected a UTCTime luna object, got unexpected constructor"

instance Luna.FromObject Time.TimeOfDay where
    fromConstructor = \case
        Luna.Constructor "TimeOfDayVal" [h, m, s] ->
            Time.TimeOfDay <$> (int  <$> Luna.fromData h)
                           <*> (int  <$> Luna.fromData m)
                           <*> (pico <$> Luna.fromData s)
        _ -> Luna.throw
            "Expected a TimeOfDay Luna object, got unexpected constructor"

instance Luna.FromObject Time.TimeZone where
    fromConstructor = \case
        Luna.Constructor "TimeZoneVal" [mins, summer, name] ->
            Time.TimeZone <$> (int <$> Luna.fromData mins)
                          <*> Luna.fromData summer
                          <*> (convert <$> Luna.fromData @Text name)
        _ -> Luna.throw
            "Excpected a TimeZone Luna object, got unexpected constructor"

instance Luna.FromObject Time.ZonedTime where
    fromConstructor = \case
        Luna.Constructor "TimeVal" [days, tod, tz] ->
            Time.ZonedTime <$> localTime <*> Luna.fromData tz where
                localTime = Time.LocalTime <$> Luna.fromData days
                                           <*> Luna.fromData tod
        _ -> Luna.throw
            "Expected a Time Luna object, got unexpected constructor"


instance Luna.ToData Time.NominalDiffTime where
    toData imps nDiffTime = Luna.toData imps (realToFrac nDiffTime :: Time.DiffTime)

instance Luna.ToData Time.Day where
    toData imps day = Luna.toData imps $ Time.toModifiedJulianDay day


instance Luna.ToObject Time.DiffTime where
    toConstructor imps diffTime =
        Luna.Constructor "TimeInterval"
                         [Luna.toData imps $ Time.diffTimeToPicoseconds diffTime]

instance Luna.ToObject Time.UTCTime where
    toConstructor imps (Time.UTCTime days diff) =
        Luna.Constructor "UTCTimeVal"
                         [ Luna.toData imps days
                         , Luna.toData imps diff ]

instance Luna.ToObject Time.TimeOfDay where
    toConstructor imps (Time.TimeOfDay h m s) =
        Luna.Constructor "TimeOfDayVal"
                         [ Luna.toData imps (integer h)
                         , Luna.toData imps (integer m)
                         , Luna.toData imps $ real s ]

instance Luna.ToObject Time.TimeZone where
    toConstructor imps (Time.TimeZone mins summer name) =
        Luna.Constructor "TimeZoneVal"
                         [ Luna.toData imps (integer mins)
                         , Luna.toData imps summer
                         , Luna.toData imps (convert name :: Text) ]

instance Luna.ToObject Time.ZonedTime where
    toConstructor imps (Time.ZonedTime (Time.LocalTime days tod) tz) =
        Luna.Constructor "TimeVal" [ Luna.toData imps days
                                   , Luna.toData imps tod
                                   , Luna.toData imps tz ]
