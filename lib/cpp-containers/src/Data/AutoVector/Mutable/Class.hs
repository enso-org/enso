module Data.AutoVector.Mutable.Class where

import Prologue

-- === Static === --

class StaticLength      a where staticLength   :: a -> Int
class StaticSize        a where staticSize     :: a -> Int
class StaticGrow      m a where staticGrow     :: a -> m a


-- === Dynamic === --

class Empty      a where empty       :: a
class Length   m a where length      :: a -> m Int
class Size     m a where size        :: a -> m Int
class New      m a where new         :: Int -> m a
class Free     m a where free        :: a -> m ()
class Read     m a where unsafeRead  :: a -> Int -> m (Item a)
class Write    m a where unsafeWrite :: a -> Int -> Item a -> m ()
class ToList   m a where toList      :: a -> m [Item a]
class FromList m a where fromList    :: [Item a] -> m a
class Grow     m a where grow        :: a -> m ()
class PushBack m a where pushBack    :: a -> Item a -> m ()
