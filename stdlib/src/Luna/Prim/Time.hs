{-# LANGUAGE OverloadedStrings #-}

module Luna.Prim.Time where

import qualified Prelude                     as P
import           Luna.Prelude                hiding (Text)
import           Luna.IR

import           Data.Fixed                  (Pico)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text.Lazy              (Text)
import           Data.Time                   (DiffTime)
import qualified Data.Time                   as Time
import qualified Data.Time.Calendar          as Time
import qualified Data.Time.Format            as Time
import           Luna.Builtin.Data.Function  (Function)
import           Luna.Builtin.Data.Module    (Imports, getObjectMethodMap)
import           Luna.Builtin.Data.LunaValue (LunaData (LunaObject), Constructor (..), Object (..), constructor, tag, fields, force')
import           Luna.Builtin.Data.LunaEff   (LunaEff, throw)
import           Luna.Builtin.Prim           (toLunaValue, ToLunaData, toLunaData, FromLunaData, fromLunaData)
import           Luna.Std.Builder            (makeFunctionIO, makeFunctionPure, maybeLT, LTp (..), int, integer, real)

pico :: Double -> Pico
pico = realToFrac

exports :: Imports -> IO (Map Name Function)
exports std = do
    let intT             = LCons "Int"    []
        tuple3T t1 t2 t3 = LCons "Tuple3" [t1, t2, t3]

    let primGetCurrentTimeVal :: IO Time.ZonedTime
        primGetCurrentTimeVal = Time.getZonedTime
    primGetCurrentTime <- makeFunctionIO (toLunaValue std primGetCurrentTimeVal) [] $ LCons "Time" []

    let primGetCurrentTimeZoneVal :: IO Time.TimeZone
        primGetCurrentTimeZoneVal = Time.getCurrentTimeZone
    primGetCurrentTimeZone <- makeFunctionIO (toLunaValue std primGetCurrentTimeZoneVal) [] $ LCons "TimeZone" []

    let primTimeToUTCVal = toLunaValue std Time.zonedTimeToUTC
    primTimeToUTC <- makeFunctionPure primTimeToUTCVal ["Time"] "UTCTime"

    let primTimeFromUTCVal = toLunaValue std Time.utcToZonedTime
    primTimeFromUTC <- makeFunctionPure primTimeFromUTCVal ["TimeZone", "UTCTime"] "Time"

    let primDiffTimesVal = toLunaValue std Time.diffUTCTime
    primDiffTimes <- makeFunctionPure primDiffTimesVal ["UTCTime", "UTCTime"] "TimeInterval"

    let fmtTime :: Text -> Time.ZonedTime -> Text
        fmtTime fmt = convert . Time.formatTime Time.defaultTimeLocale (convert fmt :: P.String)
    primFormatTime <- makeFunctionPure (toLunaValue std fmtTime) ["Text", "Time"] "Text"

    let fmtUTCTime :: Text -> Time.UTCTime -> Text
        fmtUTCTime fmt = convert . Time.formatTime Time.defaultTimeLocale (convert fmt :: P.String)
    primFormatUTCTime <- makeFunctionPure (toLunaValue std fmtUTCTime) ["Text", "UTCTime"] "Text"

    let primTimesEqVal = toLunaValue std ((==) :: Time.UTCTime -> Time.UTCTime -> Bool)
    primTimesEq <- makeFunctionPure primTimesEqVal ["UTCTime", "UTCTime"] "Bool"

    let primAddUTCTimeVal = toLunaValue std Time.addUTCTime
        primSubUTCTimeVal = toLunaValue std (\d t -> Time.addUTCTime (-d) t)
    primAddUTCTime <- makeFunctionPure primAddUTCTimeVal ["TimeInterval", "UTCTime"] "UTCTime"
    primSubUTCTime <- makeFunctionPure primSubUTCTimeVal ["TimeInterval", "UTCTime"] "UTCTime"

    let primTimeOfDayVal :: Time.DiffTime -> (Integer, Integer, Double)
        primTimeOfDayVal t = let Time.TimeOfDay h m s = Time.timeToTimeOfDay t in (convert h, convert m, realToFrac s)
    primTimeOfDay <- makeFunctionPure (toLunaValue std primTimeOfDayVal) [LCons "TimeInterval" []] $ tuple3T intT intT intT

    let primTimeOfYearVal :: Time.Day -> (Integer, Integer, Integer)
        primTimeOfYearVal t = let (y, m, d) = Time.toGregorian t in (y, integer m, integer d)
    primTimeOfYear <- makeFunctionPure (toLunaValue std primTimeOfYearVal) [intT] $ tuple3T intT intT intT

    let primFromTimeOfYearVal :: Integer -> Integer -> Integer -> Time.Day
        primFromTimeOfYearVal y m d = Time.fromGregorian y (int m) (int d)
    primFromTimeOfYear <- makeFunctionPure (toLunaValue std primFromTimeOfYearVal) [intT, intT, intT] intT

    let primMonthLengthVal :: Integer -> Integer -> Integer
        primMonthLengthVal y m = integer $ Time.gregorianMonthLength y (int m)
    primMonthLength <- makeFunctionPure (toLunaValue std primMonthLengthVal) [intT, intT] intT

    let parseTime :: Text -> Text -> Maybe Time.ZonedTime
        parseTime fmt str = Time.parseTime Time.defaultTimeLocale (convert fmt) (convert str)
    primParseTime <- makeFunctionPure (toLunaValue std parseTime) ["Text", "Text"] (maybeLT "Time")

    let primIntMilisecondsVal :: Integer -> Time.DiffTime
        primIntMilisecondsVal = (/ 1000) . realToFrac
    primIntMiliseconds <- makeFunctionPure (toLunaValue std primIntMilisecondsVal) ["Int"] "TimeInterval"

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

instance FromLunaData Time.DiffTime where
    fromLunaData dt = let errorMsg = "Expected a TimeInterval luna object, got unexpected constructor" in
        force' dt >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeInterval" -> fmap Time.picosecondsToDiffTime . fromLunaData . head $ obj ^. constructor . fields
                _              -> throw errorMsg
            _  -> throw errorMsg

instance FromLunaData Time.NominalDiffTime where
    fromLunaData dt = realToFrac <$> (fromLunaData dt :: LunaEff Time.DiffTime)

instance FromLunaData Time.Day where
    fromLunaData d = Time.ModifiedJulianDay <$> fromLunaData d

instance FromLunaData Time.UTCTime where
    fromLunaData t = let errorMsg = "Expected a UTCTime luna object, got unexpected constructor" in
        force' t >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "UTCTimeVal" -> Time.UTCTime <$> fromLunaData days <*> fromLunaData diff where [days, diff] = obj ^. constructor . fields
                _            -> throw errorMsg
            _ -> throw errorMsg

instance FromLunaData Time.TimeOfDay where
    fromLunaData tod = let errorMsg = "Expected a TimeOfDay Luna object, got unexpected constructor" in
        force' tod >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeOfDayVal" -> Time.TimeOfDay <$> (int <$> fromLunaData h) <*> (int <$> fromLunaData m) <*> (pico <$> fromLunaData s)
                                      where [h, m, s] = obj ^. constructor . fields
                _              -> throw $ (show $ obj ^. constructor . tag) <> errorMsg
            _ -> throw errorMsg

instance FromLunaData Time.TimeZone where
    fromLunaData tz = let errorMsg = "Excpected a TimeZone Luna object, got unexpected constructor" in
        force' tz >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeZoneVal" -> Time.TimeZone <$> (int <$> fromLunaData mins) <*> fromLunaData summer <*> ((convert :: Text -> P.String) <$> fromLunaData name)
                                     where [mins, summer, name] = obj ^. constructor . fields
                _             -> throw errorMsg
            _ -> throw errorMsg

instance FromLunaData Time.ZonedTime where
    fromLunaData zt = let errorMsg = "Expected a Time Luna object, got unexpected constructor" in
        force' zt >>= \case
            LunaObject obj -> case obj ^. constructor . tag of
                "TimeVal" -> Time.ZonedTime <$> localTime <*> (fromLunaData tz)
                                      where [days, tod, tz] = obj ^. constructor . fields
                                            localTime       = Time.LocalTime <$> fromLunaData days <*> fromLunaData tod
                _              -> throw errorMsg
            _ -> throw errorMsg

instance ToLunaData Time.DiffTime where
    toLunaData imps diffTime = LunaObject $
        Object (Constructor "TimeInterval" [toLunaData imps $ Time.diffTimeToPicoseconds diffTime])
               (getObjectMethodMap "TimeInterval" imps)

instance ToLunaData Time.NominalDiffTime where
    toLunaData imps nDiffTime = toLunaData imps (realToFrac nDiffTime :: Time.DiffTime)

instance ToLunaData Time.Day where
    toLunaData imps day = toLunaData imps $ Time.toModifiedJulianDay day

instance ToLunaData Time.UTCTime where
    toLunaData imps (Time.UTCTime days diff) = LunaObject $
        Object (Constructor "UTCTimeVal" [toLunaData imps days, toLunaData imps diff])
               (getObjectMethodMap "UTCTime" imps)

instance ToLunaData Time.TimeOfDay where
    toLunaData imps (Time.TimeOfDay h m s) = LunaObject $
        Object (Constructor "TimeOfDayVal" [toLunaData imps (integer h), toLunaData imps (integer m), toLunaData imps $ real s])
               (getObjectMethodMap "TimeOfDay" imps)

instance ToLunaData Time.TimeZone where
    toLunaData imps (Time.TimeZone mins summer name) = LunaObject $
        Object (Constructor "TimeZoneVal" [toLunaData imps (integer mins), toLunaData imps summer, toLunaData imps (convert name :: Text)])
               (getObjectMethodMap "TimeZone" imps)

instance ToLunaData Time.ZonedTime where
    toLunaData imps (Time.ZonedTime (Time.LocalTime days tod) tz) = LunaObject $
        Object (Constructor "TimeVal" [toLunaData imps days, toLunaData imps tod, toLunaData imps tz])
               (getObjectMethodMap "Time" imps)
