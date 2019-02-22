{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.Mutable.Class where

import Prologue hiding (Read, unsafeRead)

import qualified Memory as Memory



-- === Memory management === --

class New          m a where new          :: m a
class PlacementNew m a where placementNew :: Memory.Ptr (Memory.Management a) a -> m a
class Alloc        m a where alloc        :: Int -> m a
class Free         m a where free         :: a -> m ()
class Grow         m a where grow         :: a -> m ()


-- === Properties === --

class Size         m a where size        :: a -> m Int
class Capacity     m a where capacity    :: a -> m Int


-- === Element management === --

class Read         m a where unsafeRead  :: a -> Int -> m (Item a)
class Write        m a where unsafeWrite :: a -> Int -> Item a -> m ()
class PushBack     m a where pushBack    :: a -> Item a -> m ()
class PushFront    m a where pushFront   :: a -> Item a -> m ()
class Insert       m a where insert      :: a -> Item a -> m ()
class Remove       m a where remove      :: a -> Item a -> m ()
class InsertAt     m a where insertAt    :: a -> Int -> Item a -> m ()
class RemoveAt     m a where removeAt    :: a -> Int -> m ()

class Map m a where mapM :: a -> (Item a -> m (Item a)) -> m ()


-- === Conversions === --

class ToList       m a where toList      :: a -> m [Item a]
class FromList     m a where fromList    :: [Item a] -> m a


-- === Utils === --

unsafeWriteFromList :: (Write m a, Applicative m) => a -> [Item a] -> m ()
unsafeWriteFromList = \a -> zipWithM_ (unsafeWrite a) [0..]
{-# INLINE unsafeWriteFromList #-}

type IxMap m a = (Read m a, Write m a, Monad m)

unsafeMapAtM :: IxMap m a => a -> Int -> (Item a -> m (t, Item a)) -> m t
unsafeMapAtM = \a ix f -> do
    el <- unsafeRead a ix
    (!t, !el') <- f el
    unsafeWrite a ix el'
    pure t
{-# INLINE unsafeMapAtM #-}

unsafeMapAtM_ :: IxMap m a => a -> Int -> (Item a -> m (Item a)) -> m ()
unsafeMapAtM_ = \a ix f -> unsafeWrite a ix =<< f =<< unsafeRead a ix
{-# INLINE unsafeMapAtM_ #-}

unsafeMapAt_ :: IxMap m a => a -> Int -> (Item a -> Item a) -> m ()
unsafeMapAt_ = \a ix f -> unsafeMapAtM_ a ix (pure . f)
{-# INLINE unsafeMapAt_ #-}

