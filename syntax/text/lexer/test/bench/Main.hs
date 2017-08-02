{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prologue as P
import Criterion.Main
import Luna.Syntax.Text.Lexer
import System.Random
import System.IO (hFlush, hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.VectorText (VectorText)
import System.TimeIt

import qualified Data.Char as Char

eval :: NFData a => a -> IO a
eval = evaluate . force

env' :: NFData env => env -> (env -> Benchmark) -> Benchmark
env' = env . return

liftExp :: (Int -> a) -> (Int -> a)
liftExp f = f . (10^)

expCodeGen :: (NFData a, Show a) => (Int -> a) -> (Int -> IO a)
expCodeGen f i = do
    putStrLn $ "generating input code (10e" <> show i <> " chars)"
    out <- eval $ liftExp f i
    putStrLn "code generated sucessfully"
    return out

maxExpCodeLen :: Int
maxExpCodeLen = 4

expCodeGenBench  :: (Int -> VectorText) -> Int -> Benchmark
expCodeGenBenchs :: (Int -> VectorText) -> [Benchmark]
expCodeGenBench  f i = env (expCodeGen f i) $ bench ("10e" <> show i) . nf (runLexer @String)
expCodeGenBenchs f   = expCodeGenBench f <$> [4..maxExpCodeLen]


mkCodeRandom :: Int -> VectorText
mkCodeRandom i = convert . P.take i $ Char.chr <$> randomRs (32,100) (mkStdGen 0)

mkCodeTerminators :: Int -> VectorText
mkCodeTerminators i = convert $ replicate i ';'


main = do
    hSetBuffering stdout NoBuffering
    -- putStrLn "Preparing code samples"
    -- rcodes <- expCodes maxExpSize mkCodeRandom
    -- rcodes <- eval $ mkCodeRandom . (10^) <|$> [1..maxExpSize] :: IO [(Int, VectorText)]
    -- rc <- eval $ mkCodeRandom 100000
    -- putStrLn "xxxx---"
    -- timeIt $ eval $ runLexer @String rc
    -- timeIt $ eval $ runLexer @String rc
    -- timeIt $ eval $ runLexer @String rc
    -- timeIt $ eval $ runLexer @String rc
    defaultMain
        [ bgroup "terminators" $ expCodeGenBenchs mkCodeTerminators
        -- , bgroup "random code" $ expCodeGenBenchs mkCodeRandom
        ]
