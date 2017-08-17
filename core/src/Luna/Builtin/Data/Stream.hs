{-# LANGUAGE UndecidableInstances #-}

module Luna.Builtin.Data.Stream where

import Prologue_old hiding (Monoid, mappend, mempty, (<>), force)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Identity
import Data.Monoid

data Future a = Future { force :: IO a }

instance Functor Future where
   fmap f (Future get) = Future $ f <$> get

instance Applicative Future where
   pure x = Future $ pure x
   Future getF <*> Future getV = Future $ getF <*> getV

instance Monad Future where
   Future getV >>= f = Future $ getV >>= force . f

instance Monoid (Future a) where
   mempty  = never
   mappend = race

mkFuture :: IO (Future a, a -> IO ())
mkFuture = do
   v <- newEmptyMVar
   return (Future (readMVar v), putMVar v)

never :: Future a
never = Future $ do
   m <- newEmptyMVar
   takeMVar m

race :: Future a -> Future a -> Future a
race (Future a) (Future b) = Future $ do
   sync <- newEmptyMVar
   let run fut = forkIO $ do
         v <- fut
         putMVar sync v

   tida <- run a
   tidb <- run b
   res  <- readMVar sync
   killThread tida
   killThread tidb
   return res

newtype Stream a = Stream { runStream :: (a, Future (Stream a)) }

instance Functor Stream where
   fmap f (Stream (val, fut)) = Stream (f val, fmap f <$> fut)

instance Applicative Stream where
   pure s = Stream (s, never)
   sigF@(Stream (f, futF)) <*> sigV@(Stream (a, futA)) = Stream (f a, ((<*> sigV) <$> futF) <> ((sigF <*>) <$> futA))

stepper :: a -> Future (Stream a) -> Stream a
stepper init ev = Stream (init, ev)

switcher :: Stream a -> Future (Stream (Stream a)) -> Stream a
switcher r rs = joinS $ r `stepper` rs

joinS :: Stream (Stream a) -> Stream a
joinS (Stream (Stream (a, ur), urr)) = Stream (a, ((`switcher` urr) <$> ur) <> (joinS <$> urr))

instance Monad Stream  where
   sigV >>= f = joinS $ f <$> sigV

mapIO :: (a -> IO ()) -> Stream a -> IO ()
mapIO action (Stream (val, fut)) = do
   action val
   nextSig <- force fut
   mapIO action nextSig
