{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}

module Data.Hetero.Stack where

import Prelude hiding (head, tail)

import Control.Lens.Utils hiding (cons)
import Data.Construction
import Data.Default
import Data.Monoid
import Data.Property
import Data.Repr


-------------------
-- === Stack === --
-------------------

data Stack (t :: * -> *) layers where
    Layer :: t l -> Stack t ls -> Stack t (l ': ls)
    Null  :: Stack t '[]


-- === Utils === --

head :: Lens' (Stack t (l ': ls)) (t l)
head = lens (\(Layer a _) -> a) (\(Layer _ s) a -> Layer a s) ; {-# INLINE head #-}

tail :: Lens' (Stack t (l ': ls)) (Stack t ls)
tail = lens (\(Layer _ s) -> s) (\(Layer a _) s -> Layer a s) ; {-# INLINE tail #-}

push :: t l -> Stack t ls -> Stack t (l ': ls)
push = Layer ; {-# INLINE push #-}

pop :: Stack t (l ': ls) -> (t l, Stack t ls)
pop (Layer el s) = (el, s) ; {-# INLINE pop #-}


-- === HasLayers === --

class                                          HasLayer l ls        where layer :: forall t. Lens' (Stack t ls) (t l)
instance {-# OVERLAPPABLE #-}                  HasLayer l (l ': ls) where layer = head              ; {-# INLINE layer #-}
instance {-# OVERLAPPABLE #-} HasLayer l ls => HasLayer l (t ': ls) where layer = tail . layer ; {-# INLINE layer #-}


-- === Instances === --

-- Show
instance                                           Show (Content (Stack t '[]))       where show _                        = ""                              ; {-# INLINE show #-}
instance {-# OVERLAPPING #-} Show (t l)         => Show (Content (Stack t '[l]))      where show (unwrap' -> Layer l ls) = show l                           ; {-# INLINE show #-}
instance (Show (t l), ContentShow (Stack t ls)) => Show (Content (Stack t (l ': ls))) where show (unwrap' -> Layer l ls) = show l <> ", " <> contentShow ls ; {-# INLINE show #-}
instance ContentShow (Stack t ls)               => Show          (Stack t ls)         where
    showsPrec d s = showParen (d > app_prec) . showString $ "Stack (" <> contentShow s <> ")"
        where app_prec = 10
    {-# INLINE showsPrec #-}

-- Properties
type instance Access p (Stack t ls) = t p

instance {-# OVERLAPPABLE #-}                             Accessor p (Stack t (p ': ls)) where access    (Layer t _) = t                     ; {-# INLINE access  #-}
instance {-# OVERLAPPABLE #-} Accessor p (Stack t ls)  => Accessor p (Stack t (l ': ls)) where access    (Layer _ l) = access @p l           ; {-# INLINE access  #-}
instance {-# OVERLAPPABLE #-}                             Updater' p (Stack t (p ': ls)) where update' a (Layer _ s) = Layer a s             ; {-# INLINE update' #-}
instance {-# OVERLAPPABLE #-} Updater' p (Stack t ls)  => Updater' p (Stack t (l ': ls)) where update' a (Layer t s) = Layer t (update' a s) ; {-# INLINE update' #-}

-- Constructor
instance ( Constructor a m (t l)
         , Constructor a m (Stack t ls)) => Constructor a m (Stack t (l ': ls)) where cons a = Layer <$> cons a <*> cons a ; {-# INLINE cons #-}
instance Monad m                         => Constructor a m (Stack t '[]      ) where cons _ = return Null                 ; {-# INLINE cons #-}

-- Default
instance layers ~ '[] => Default (Stack t layers) where
    def = Null ; {-# INLINE def #-}
