module Luna.Compilation.Pass.Interpreter.Genetic where

import           Prelude (read)
import           Prelude.Luna
import           Luna.Compilation.Pass.Interpreter.Value
import qualified Data.Map                                as Map
import           Unsafe.Coerce

data Transcript = Transcript { feature      :: String
                             , start        :: Int
                             , end          :: Int
                             , score        :: Int
                             , strand       :: String
                             , geneId       :: String
                             , transcriptId :: String
                             , fpkm         :: Double
                             , confLo       :: Double
                             , confHi       :: Double
                             , cov          :: Double
                             } deriving (Show, Eq)

instance ToData   Transcript where unsafeToData = Boxed . Object transcriptClass . unsafeCoerce
instance FromData Transcript where unsafeFromData (Boxed (Object _ d)) = unsafeCoerce d

transcriptClass :: ClassDescription
transcriptClass = ClassDescription $ Map.fromList
    [ ("feature",      toMethodBoxed feature)
    , ("start",        toMethodBoxed start)
    , ("end",          toMethodBoxed end)
    , ("score",        toMethodBoxed score)
    , ("strand",       toMethodBoxed strand)
    , ("geneId",       toMethodBoxed geneId)
    , ("transcriptId", toMethodBoxed transcriptId)
    , ("fpkm",         toMethodBoxed fpkm)
    , ("confLo",       toMethodBoxed confLo)
    , ("confHi",       toMethodBoxed confHi)
    , ("cov",          toMethodBoxed cov)
    ]

parseTranscript :: String -> LunaM Transcript
parseTranscript s = return $ Transcript (fs !! 2)
                               (read $ fs !! 3)
                               (read $ fs !! 4)
                               (read $ fs !! 5)
                               (fs !! 6)
                               (fs !! 9)
                               (fs !! 11)
                               (read $ read $ fs !! (15 - correction))
                               (read $ read $ fs !! (19 - correction))
                               (read $ read $ fs !! (21 - correction))
                               (read $ read $ fs !! (23 - correction)) where
    fs = words $ filter (/= ';') s
    correction = if fs !! 2 == "transcript" then 2 else 0

showTranscript :: Transcript -> String
showTranscript (Transcript feat st end sc str gid tid fpkm lo hi cov) =  "feature: "      <> feat      <> "\n"
                                                                      <> "start: "        <> show st   <> "\n"
                                                                      <> "end: "          <> show end  <> "\n"
                                                                      <> "score: "        <> show sc   <> "\n"
                                                                      <> "strand: "       <> str       <> "\n"
                                                                      <> "geneId: "       <> gid       <> "\n"
                                                                      <> "transcriptId: " <> tid       <> "\n"
                                                                      <> "FPKM: "         <> show fpkm <> "\n"
                                                                      <> "confLo: "       <> show lo   <> "\n"
                                                                      <> "confHi: "       <> show hi   <> "\n"
                                                                      <> "cov: "          <> show cov
