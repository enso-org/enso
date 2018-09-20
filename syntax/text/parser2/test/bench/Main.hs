{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}

module Main where

import Prologue hiding (many, seq)

import qualified Data.Attoparsec.Text as Attoparsec
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import qualified Text.Parsers.Frisby  as Frisby

import Criterion.Measurement (initializeTime)
import Data.Set              (Set)
import System.IO             (BufferMode (NoBuffering), hSetBuffering, stdout)

import Criterion
import Criterion.Main


--------------------------------
-- === Running benchmarks === --
--------------------------------

iters :: Int
iters = 100000

src1 :: Text
src1 = Text.replicate iters "test" <> "x"

src1_str :: String
src1_str = concat $ replicate iters "test"

src1_attoparsec :: Attoparsec.Parser Text
src1_attoparsec = let
    p1 = Attoparsec.takeWhile1
       $ \c -> c == 't' || c == 'e' || c == 's' || c == 't'
    -- p2 = Attoparsec.char 'x'
    -- in (\a b -> a <> Text.singleton b) <$> p1 <*> p2
    in p1

src1_frisby :: Frisby.PM s (Frisby.P s [Char])
src1_frisby = Frisby.newRule $ Frisby.many $ Frisby.oneOf ['t', 'e', 's', 't']

src1_man1 :: [Char] -> Maybe [Char]
src1_man1 = src1_man1' []

src1_man1' :: [Char] -> [Char] -> Maybe [Char]
src1_man1' = go where
    tst c = c == 't' || c == 'e' || c == 's' || c == 't'
    go rs = \case
        [] -> Just rs
        (a:as) -> if tst a then go (a:rs) as else Nothing
{-# INLINE src1_man1' #-}

src1_man2 :: Text -> Maybe Text
src1_man2 src = out where
    tst = \c -> c == 't' || c == 'e' || c == 's' || c == 't'
    rs  = Text.takeWhile tst src
    out = Just rs

data Grammar a
    = Tokens !(Set a) !(a -> Bool)
    | Many   !(Grammar a)
    | Seq    !(Grammar a) !(Grammar a)
    | X      !(Grammar a)

-- instance Mempty (Grammar a) where
--     mempty = Tokens 0 $ const False
--     {-# INLINE mempty #-}

instance Ord a => Semigroup (Grammar a) where
    Tokens s f <> Tokens s' g = Tokens (s <> s') $ \c -> f c || g c
    {-# INLINE (<>) #-}

token :: Eq a => a -> Grammar a
token = \a -> Tokens (Set.singleton a) (a ==)
{-# INLINE token #-}

many :: Grammar a -> Grammar a
many = Many
{-# INLINE many #-}

seq :: Grammar a -> Grammar a -> Grammar a
seq = Seq
{-# INLINE seq #-}

data Result
    = Success Text Text
    | Fail
    deriving (Show, Generic)

instance NFData Result

runTokenParser :: Grammar Char -> Text -> Result
runTokenParser = \grammar stream -> case grammar of
    Tokens _ tst -> let
        head = Text.head stream
        in if tst head
            then Success (Text.tail stream) (Text.singleton head)
            else Fail
    Many (Tokens _ tst) -> let
        (!consumed, !rest) = Text.span tst stream
        in Success rest consumed
    Seq !grammar1 _ -> runTokenParser grammar1 stream
    X !grammar -> runTokenParser grammar stream
        -- Fail               -> Fail
        -- Success !stream' !ok -> case runTokenParser grammar2 stream' of
        --     Fail                -> Fail
        --     Success !stream'' !ok' -> Success stream'' $! ok

-- {-# INLINE runTokenParser #-}

-- (Tokens _ tst) src = out where
--     rs  = Text.takeWhile tst src
--     out = Just rs

src1_man3 :: Text -> Result
src1_man3 src = out where
    s1 = token 't'
    s2 = token 'e'
    s3 = token 's'
    s4 = token 't'
    -- tst = s1 <> s2 <> s3 <> s4
    tst = many $! foldl (<>) s1 [s2, s3, s4]
    tst' = X tst -- !!!!! Adding X makes it 10 times slower
    out = runTokenParser tst' src

-- data CList
--     = CCons !Char CList
--     | CNil

-- elemx :: Char -> CList -> Bool
-- elemx = \c -> let
--     go = \case
--         CNil -> False
--         (CCons a as) -> if c == a then True else go as
--     in go
-- {-# INLINE elemx #-}


-----------------------------


data Tokens2 a = Tokens2 !(Set a) !(a -> Bool)
data Many2   a = Many2   !a
data Seq2    a b = Seq2  !a !b
data X2      a = X2      !a


token2 :: Eq a => a -> Tokens2 a
token2 = \a -> Tokens2 (Set.singleton a) (a ==)
{-# INLINE token2 #-}

many2 :: a -> Many2 a
many2 = Many2
{-# INLINE many2 #-}

seq2 :: a -> b -> Seq2 a b
seq2 = Seq2
{-# INLINE seq2 #-}

class ParserRunner a where
    parserRunner :: a -> Text -> Result

instance Ord a => Semigroup (Tokens2 a) where
    Tokens2 s f <> Tokens2 s' g = Tokens2 (s <> s') $ \c -> f c || g c
    {-# INLINE (<>) #-}

instance ParserRunner (Tokens2 Char) where
    parserRunner = \(Tokens2 _ tst) stream -> let
        head = Text.head stream
        in if tst head
            then Success (Text.tail stream) (Text.singleton head)
            else Fail
    {-# INLINE parserRunner #-}

instance ParserRunner (Many2 (Tokens2 Char)) where
    parserRunner = \(Many2 (Tokens2 _ tst)) stream -> let
        (!consumed, !rest) = Text.span tst stream
        in Success rest consumed
    {-# INLINE parserRunner #-}

instance ParserRunner a => ParserRunner (X2 a) where
    parserRunner = \(X2 a) stream -> parserRunner a stream
    {-# INLINE parserRunner #-}

src1_man4 :: Text -> Result
src1_man4 src = out where
    s1 = token2 't'
    s2 = token2 'e'
    s3 = token2 's'
    s4 = token2 't'
    tst = many2 $! s1 <> s2 <> s3 <> s4
    tst' = X2 tst
    out = parserRunner tst' src


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    defaultMain
        [ bgroup "CppContainers"
            [ env (pure src1)     (\(~src) -> bench "a1" $ nf (Attoparsec.parseOnly src1_attoparsec) src)
            -- , env (pure src1_str) (\(~src) -> bench "f1" $ nf (Frisby.runPeg src1_frisby) src)
            -- , env (pure src1_str) (\(~src) -> bench "m1" $ nf src1_man1 src)
            -- , env (pure src1)     (\(~src) -> bench "m2" $ nf src1_man2 src)
            , env (pure src1)     (\(~src) -> bench "m3" $ nf src1_man3 src)
            , env (pure src1)     (\(~src) -> bench "m4" $ nf src1_man4 src)
            ]
        ]
    -- --             $ nfIO (SetTest.testInsertAndLookupIntSet $ 10^i)) <$> expSizes
    -- --         , bgroup "Data.PtrSet.Cpp"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ nfIO (SetTest.testInsertAndLookupCSet $ 10^i)) <$> expSizes
    -- --         , bgroup "Test.Data.PtrSet.Cpp"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ nfIO (SetTest.testInsertAndLookupForeignSet $ 10^i)) <$> expSizes
    -- --         , bgroup "testWithArrayLen"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ nfIO (SetTest.testWithArrayLen $ 10^i)) <$> expSizes
    -- --         , bgroup "FFICost"
    -- --             [ bgroup "Data.PtrSet.Cpp"
    -- --                 $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --                 $ nfIO (SetTest.testInsertLookupOrderedCSet $ 10^i)) <$> expSizes
    -- --             , bgroup "Test.Data.PtrSet.Cpp"
    -- --                 $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --                 $ nfIO (SetTest.testInsertLookupOrderedForeignSet $ 10^i)) <$> expSizes
    -- --             ]
    -- --         ]
    --     [ bgroup "IR"
    --         -- [ bgroup "Create node"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Basic.test_createNode (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         -- , bgroup "Malloc ptr"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Basic.test_mallocPtr (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         [ bgroup "ghc 8.4 bug"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.ghc84bug (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         , bgroup "ghc 8.4 bug 2"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.ghc84bug2 (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         , bgroup "Read/Write Layer + State config 4"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer4 (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         -- , bgroup "Read/Write Layer + State config 3"
    --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    --         --     $ perRunEnv (return ())
    --         --     $ \v -> Basic.test_readWriteLayer3 (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         , bgroup "Read/Write Layer + State config 2"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer2 (10 ^ i))  <$> [minExpVec..maxExpVec]

    --         , bgroup "Read/Write Layer + State"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Basic.test_readWriteLayer_ptrBuffOff (10 ^ i))  <$> [minExpVec..maxExpVec]

    -- --
    -- --         [ bgroup "Vector creation Hardcoded"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_VectorCreationHardcoded (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "List creation Hardcoded"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_ListCreationHardcoded (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Vector creation"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_VectorCreation (10 ^ i))  <$> [5]
    -- --
    -- --         , bgroup "List creation"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_ListCreation (10 ^ i))  <$> [5]
    -- --
    -- --         , bgroup "Pure loop"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.pureLoop (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "TS3 X"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TS3.pureLoop_X (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "TS3 Z"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TS3.pureLoop_Z (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Tup X"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TT.pureLoop_X2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Tup Z"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TT.pureLoop_Z2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "GADTs X"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TT.pureLoop_X (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "GADTs Z"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> TT.pureLoop_Z (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write Layer static"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Basic.test_readWriteLayer_static (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write Layer + State config from Ptr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Basic.test_readWriteLayer_ptrOff (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write Layer + State config from BuffPtr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Basic.test_readWriteLayer_ptrBuffOff (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    --         , bgroup "Read/Write Ptr"
    --             $ (\(i :: Int) -> bench ("10e" <> show i)
    --             $ perRunEnv (return ())
    --             $ \v -> Test.readWritePtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write T Ptr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWritePtr_T (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write T Ptr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWritePtr_T2 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write IORef"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write T IORef"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteIORef_T (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Read/Write Layer"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Basic.test_readWriteLayer (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    --         ]
    --         ]
    -- --     , bgroup "Storable"
    -- --         [ bgroup "Single storable"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_singleStorable (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         , bgroup "Partial storable"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_partialStorable (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         ]
    -- --     , bgroup "Construction"
    -- --         -- [ bgroup "IORefU"
    -- --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --         --     $ perRunEnv (return ())
    -- --         --     $ \v -> Test.test_newIORefU (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         --
    -- --         -- , bgroup "IORef"
    -- --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --         --     $ perRunEnv (return ())
    -- --         --     $ \v -> Test.test_newIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         [ bgroup "ForeignPtr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_mallocForeignPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         --
    -- --         -- ,  bgroup "StablePtr"
    -- --         --     $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --         --     $ perRunEnv (return ())
    -- --         --     $ \v -> Test.test_mallocSPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         --
    -- --         ,  bgroup "malloc 1 (+ free)"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_malloc1 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         ,  bgroup "malloc 100 (+ free)"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.test_malloc100 (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         ]
    -- --
    -- --     , bgroup "Read+Write"
    -- --         [ bgroup "Pure loop"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.pureLoop (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "IORef"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteIORef (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "IORefU"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteIORefU (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "ForeignPtr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteForeignPtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Vector"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWriteVector (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "Ptr"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \v -> Test.readWritePtr (10 ^ i))  <$> [minExpVec..maxExpVec]
    -- --         ]
    -- --
    -- --     , bgroup "Fill"
    -- --         [ bgroup "MAutoVector with Int"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (return ())
    -- --             $ \x -> do{v <- unsafeNew (10^i + 1); Test.fillMAutoVector_Int (10 ^ i) v})  <$> [minExpVec..maxExpVec]
    -- --
    -- --         , bgroup "MVector with Int"
    -- --             $ (\(i :: Int) -> bench ("10e" <> show i)
    -- --             $ perRunEnv (Vector.unsafeNew (10 ^ i))
    -- --             $ (Test.fillMVector_Int (10 ^ i)))  <$> [minExpVec..maxExpVec]
    -- --         ]
    -- --     ]
