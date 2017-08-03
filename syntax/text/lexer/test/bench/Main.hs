{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prologue as P hiding (Symbol)
import Criterion.Main
import Luna.Syntax.Text.Lexer
import System.Random
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Text (Text)
import qualified Data.Text as Text
import System.TimeIt

import qualified Data.Char as Char

import Conduit
import Data.Char (toUpper)
import Data.Attoparsec.Text as Parser
import Luna.Syntax.Text.Lexer.Stream
import Luna.Syntax.Text.Lexer (Symbol)
import System.IO (FilePath)

import Control.Monad.State.Layered

eval :: NFData a => a -> IO a
eval = evaluate . force

liftExp :: (Int -> a) -> (Int -> a)
liftExp f = f . (10^)

expCodeGen :: (NFData a, Show a) => (Int -> a) -> (Int -> IO a)
expCodeGen f i = do
    -- putStrLn $ "generating input code (10e" <> show i <> " chars)"
    out <- eval $ liftExp f i
    -- putStrLn "code generated sucessfully"
    return out

maxExpCodeLen :: Int
maxExpCodeLen = 6

-- expCodeGenBench  :: (Int -> Text) -> Int -> Benchmark
-- expCodeGenBenchs :: (Int -> Text) -> [Benchmark]
expCodeGenBench  p f i = env (expCodeGen f i) $ bench ("10e" <> show i) . nf p
expCodeGenBenchs p f   = expCodeGenBench p f <$> [6..maxExpCodeLen]
--
--
-- mkCodeRandom :: Int -> VectorText
-- mkCodeRandom i = convert . P.take i $ Char.chr <$> randomRs (32,100) (mkStdGen 0)
--
mkCodeNumbers :: Int -> Text
mkCodeNumbers i = Text.replicate i $ convert ['0'..'9']

mkCodeTerminators, mkBigVariable :: Int -> Text
mkCodeTerminators i = Text.replicate i ";" ; {-# INLINE mkCodeTerminators #-}
mkBigVariable     i = Text.replicate i "a" ; {-# INLINE mkBigVariable     #-}

mkVariablesL1, mkVariablesL5, mkVariablesL10 :: Int -> Text
mkVariablesL1  i = Text.replicate i "a "          ; {-# INLINE mkVariablesL1  #-}
mkVariablesL5  i = Text.replicate i "abcde "      ; {-# INLINE mkVariablesL5  #-}
mkVariablesL10 i = Text.replicate i "abcdefghij " ; {-# INLINE mkVariablesL10 #-}


main = do
    return ()
    defaultMain
        --[ bgroup "big variablexx"           $ [bench "big file" $ nfIO (runLexerFromFile "/tmp/input.txt")]
        [ bgroup "big variable"             $ expCodeGenBenchs runLexerPure           mkBigVariable
        , bgroup "variables L1"             $ expCodeGenBenchs runLexerPure           mkVariablesL1
        , bgroup "variables L5"             $ expCodeGenBenchs runLexerPure           mkVariablesL5
        , bgroup "variables L10"            $ expCodeGenBenchs runLexerPure           mkVariablesL10
        , bgroup "terminators"              $ expCodeGenBenchs runLexerPure           mkCodeTerminators
        , bgroup "manual terminator parser" $ expCodeGenBenchs manualTerminatorParser mkCodeTerminators
        -- , bgroup "random code" $ expCodeGenBenchs mkCodeRandom
        ]

manualTerminatorParser :: Text -> Either String [Char]
manualTerminatorParser = parseOnly $ many (char ';') ; {-# INLINE manualTerminatorParser #-}

runLexerPure :: Text -> Either ParseError [(Span, Symbol)]
runLexerPure t = sequence
               $ runConduitPure
               $ yield t
              .| conduitParserEither (runStateT @EntryPoint lexer)
              .| sinkList
{-# INLINE runLexerPure #-}

-- runLexerPureHack :: Text -> [(Symbol, Int)]
-- runLexerPureHack t = case Parser.parse (runStateT @EntryPoint lexer) t of
--     Fail {} -> undefined
--     Partial f -> case f mempty of
--         Done rest a -> if Text.null rest then [a] else error "not all parsed"
--         _           -> undefined
--     Done rest a -> if Text.null rest then [a] else a : runLexerPureHack rest
-- {-# INLINE runLexerPureHack #-}
--
--
runLexerFromFile :: MonadIO m => FilePath -> m (Either ParseError [(Span, Symbol)])
runLexerFromFile p = liftIO
                   $ fmap sequence
                   $ runConduitRes
                   $ sourceFile p
                  .| decodeUtf8C
                  .| conduitParserEither (runStateT @EntryPoint lexer)
                  .| sinkList
{-# INLINE runLexerFromFile #-}

--
-- main :: IO ()
-- main = do
--     hSetBuffering stdout NoBuffering
--     out <- runConduitRes
--          $ sourceFile "/tmp/input.txt"
--         .| decodeUtf8C
--         .| conduitParserEither lexer
--         .| sinkList
--     print $ sequence out
--     -- .| printC
