{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude as P

import GHC.IO as X (evaluate)

import qualified Control.Monad.State.Layered as State
import qualified Control.Monad.State.Strict  as S

import Control.Monad.State.Layered (MonadStates)
import Foreign.Storable            (peek, poke)
import System.IO                   (stdout, hSetBuffering, BufferMode(..))

import Control.DeepSeq
import Control.Monad.Identity
import Criterion.Main
import Data.IORef
import Foreign.ForeignPtr

eval :: NFData a => a -> IO a
eval = evaluate . force ; {-# INLINE eval #-}

mtlIncState :: S.MonadState Int m => m ()
mtlIncState = S.modify (1+) ; {-# INLINE mtlIncState #-}
{-# SPECIALIZE mtlIncState :: S.State Int () #-}


-- | incLoop transformation needed because of bug https://ghc.haskell.org/trac/ghc/ticket/14062
incLoop  :: State.Monad  Int          m => Int -> m Int
incLoop2 :: MonadStates '[Int, Word] m => Int -> m (Int, Word)
incLoop  n = repeatM incState  n >> State.get @Int                               ; {-# INLINE incLoop  #-}
incLoop2 n = repeatM incState2 n >> ((,) <$> State.get @Int <*> State.get @Word) ; {-# INLINE incLoop2 #-}

incState  :: State.Monad  Int          m => m ()
incState2 :: MonadStates '[Int, Word] m => m ()
incState  = State.modify_ @Int (1+)                             ; {-# INLINE incState  #-}
incState2 = State.modify_ @Int (1+) >> State.modify_ @Word (1+) ; {-# INLINE incState2 #-}

repeatM :: Monad m => m a -> Int -> m ()
repeatM f = go where
    go 0 = pure ()
    go i = f >> go (i - 1)
{-# INLINE repeatM #-}

pureInc :: Int -> Int -> Int
pureInc !a !i = case i of
    0 -> a
    _ -> pureInc (a + 1) (i - 1)

pureInc2 :: Int -> Word -> Int -> (Int, Word)
pureInc2 !a !b i = case i of
    0 -> (a,b)
    _ -> pureInc2 (a + 1) (b + 1) (i - 1)

iorefInc :: Int -> IO Int
iorefInc !i = do
    !ref <- newIORef (0 :: Int)
    let go = \case
            0 -> pure ()
            j -> modifyIORef' ref (+1) >> go (j - 1)
    go i
    readIORef ref
{-# INLINE iorefInc #-}

fpInc :: Int -> IO ()
fpInc !i = do
    !(ptr :: ForeignPtr Int) <- mallocForeignPtr
    let go = \case
            0 -> return ()
            j -> do
                withForeignPtr ptr $ \ !p -> do
                    val <- peek p
                    poke p (val + 1)
                go (j - 1)
    go i
{-# INLINE fpInc #-}


sInt :: Monad m => State.StateT Int    m a -> m a
sWrd :: Monad m => State.StateT Word   m a -> m a
sStr :: Monad m => State.StateT String m a -> m a
sChr :: Monad m => State.StateT Char   m a -> m a
sTup :: Monad m => State.StateT ()     m a -> m a
sInt = flip (State.evalT @Int)    0   ; {-# INLINE sInt #-}
sWrd = flip (State.evalT @Word)   0   ; {-# INLINE sWrd #-}
sStr = flip (State.evalT @String) ""  ; {-# INLINE sStr #-}
sChr = flip (State.evalT @Char)   'x' ; {-# INLINE sChr #-}
sTup = flip (State.evalT @())     ()  ; {-# INLINE sTup #-}

t_0 :: Int -> ()
t_0 = runIdentity . sInt . repeatM incState ; {-# INLINE t_0 #-}

t_1R, t_2R, t_3R :: Int -> Int
t_1R = runIdentity . sInt . sStr               . incLoop ; {-# INLINE t_1R #-}
t_2R = runIdentity . sInt . sStr . sChr        . incLoop ; {-# INLINE t_2R #-}
t_3R = runIdentity . sInt . sStr . sChr . sTup . incLoop ; {-# INLINE t_3R #-}

t_1L, t_2L, t_3L :: Int -> Int
t_1L = runIdentity               . sStr . sInt . incLoop ; {-# INLINE t_1L #-}
t_2L = runIdentity        . sChr . sStr . sInt . incLoop ; {-# INLINE t_2L #-}
t_3L = runIdentity . sTup . sChr . sStr . sInt . incLoop ; {-# INLINE t_3L #-}

t_1R2, t_2R2, t_3R2 :: Int -> (Int,Word)
t_1R2 = runIdentity . sInt . sWrd . sStr               . incLoop2 ; {-# INLINE t_1R2 #-}
t_2R2 = runIdentity . sInt . sWrd . sStr . sChr        . incLoop2 ; {-# INLINE t_2R2 #-}
t_3R2 = runIdentity . sInt . sWrd . sStr . sChr . sTup . incLoop2 ; {-# INLINE t_3R2 #-}

t_1L2, t_2L2, t_3L2 :: Int -> (Int, Word)
t_1L2 = runIdentity               . sStr . sInt . sWrd . incLoop2 ; {-# INLINE t_1L2 #-}
t_2L2 = runIdentity        . sChr . sStr . sInt . sWrd . incLoop2 ; {-# INLINE t_2L2 #-}
t_3L2 = runIdentity . sTup . sChr . sStr . sInt . sWrd . incLoop2 ; {-# INLINE t_3L2 #-}

-- t_1R2E, t_2R2E, t_3R2E :: Int -> Either () (Int,Word)
-- t_1R2E = runIdentity . sInt . sWrd . sStr               . runEitherT . incLoop2 ; [># INLINE t_1R2E #<]
-- t_2R2E = runIdentity . sInt . sWrd . sStr . sChr        . runEitherT . incLoop2 ; [># INLINE t_2R2E #<]
-- t_3R2E = runIdentity . sInt . sWrd . sStr . sChr . sTup . runEitherT . incLoop2 ; [># INLINE t_3R2E #<]

-- t_1L2E, t_2L2E, t_3L2E :: Int -> Either () (Int, Word)
-- t_1L2E = runIdentity . runEitherT               . sStr . sInt . sWrd . incLoop2 ; [># INLINE t_1L2E #<]
-- t_2L2E = runIdentity . runEitherT        . sChr . sStr . sInt . sWrd . incLoop2 ; [># INLINE t_2L2E #<]
-- t_3L2E = runIdentity . runEitherT . sTup . sChr . sStr . sInt . sWrd . incLoop2 ; [># INLINE t_3L2E #<]
--
-- t_1R2Eb, t_2R2Eb, t_3R2Eb :: Int -> Either () (Int,Word)
-- t_1R2Eb = runIdentity . sInt . runEitherT . sWrd . sStr               . incLoop2 ; [># INLINE t_1R2Eb #<]
-- t_2R2Eb = runIdentity . sInt . runEitherT . sWrd . sStr . sChr        . incLoop2 ; [># INLINE t_2R2Eb #<]
-- t_3R2Eb = runIdentity . sInt . runEitherT . sWrd . sStr . sChr . sTup . incLoop2 ; [># INLINE t_3R2Eb #<]
--
-- t_1L2Eb, t_2L2Eb, t_3L2Eb :: Int -> Either () (Int, Word)
-- t_1L2Eb = runIdentity               . sStr . sInt . runEitherT . sWrd . incLoop2 ; [># INLINE t_1L2Eb #<]
-- t_2L2Eb = runIdentity        . sChr . sStr . sInt . runEitherT . sWrd . incLoop2 ; [># INLINE t_2L2Eb #<]
-- t_3L2Eb = runIdentity . sTup . sChr . sStr . sInt . runEitherT . sWrd . incLoop2 ; [># INLINE t_3L2Eb #<]

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    let iterCount = 100000000

    putStrLn "\nTesting time overhead of monad transformers"
    putStrLn $ "IMPORTANT: These times should be THE SAME. If they are not, "
        <> "you've broken then implementation. Go back and fix it."

    putStrLn "\n\n=== Single counter loop ===\n"
    defaultMain [ bench "IORef    (10e6)" $ nfIO (iorefInc iterCount)
                , bench "FPtr     (10e6)" $ nfIO (fpInc iterCount)
                , bench "pure     (10e6)" $ nf (pureInc 0) iterCount
                -- , bench "1R trans (10e6)" $ nf t_1R iterCount
                -- , bench "2R trans (10e6)" $ nf t_2R iterCount
                -- , bench "3R trans (10e6)" $ nf t_3R iterCount
                -- , bench "1L trans (10e6)" $ nf t_1L iterCount
                -- , bench "2L trans (10e6)" $ nf t_2L iterCount
                -- , bench "3L trans (10e6)" $ nf t_3L iterCount
                ]

    putStrLn "\n\n=== Double counter loop ===\n"
    defaultMain [ bench "pure     (10e6)" $ nf (pureInc2 0 0) iterCount
                -- , bench "1R trans (10e6)" $ nf t_1R2 iterCount
                -- , bench "2R trans (10e6)" $ nf t_2R2 iterCount
                -- , bench "3R trans (10e6)" $ nf t_3R2 iterCount
                -- , bench "1L trans (10e6)" $ nf t_1L2 iterCount
                -- , bench "2L trans (10e6)" $ nf t_2L2 iterCount
                -- , bench "3L trans (10e6)" $ nf t_3L2 iterCount
                ]

    -- putStrLn "\n\n=== Double counter loop in EitherT ===\n"
    -- defaultMain [ bench "1R trans (10e6)" $ nf t_1R2E iterCount
                -- , bench "2R trans (10e6)" $ nf t_2R2E iterCount
                -- , bench "3R trans (10e6)" $ nf t_3R2E iterCount
                -- , bench "1L trans (10e6)" $ nf t_1L2E iterCount
                -- , bench "2L trans (10e6)" $ nf t_2L2E iterCount
                -- , bench "3L trans (10e6)" $ nf t_3L2E iterCount
                -- ]

    -- putStrLn "\n\n=== Double counter loop with EitherT in-between ===\n"
    -- defaultMain [ bench "1R trans (10e6)" $ nf t_1R2Eb iterCount
                -- , bench "2R trans (10e6)" $ nf t_2R2Eb iterCount
                -- , bench "3R trans (10e6)" $ nf t_3R2Eb iterCount
                -- , bench "1L trans (10e6)" $ nf t_1L2Eb iterCount
                -- , bench "2L trans (10e6)" $ nf t_2L2Eb iterCount
                -- , bench "3L trans (10e6)" $ nf t_3L2Eb iterCount
                -- ]
