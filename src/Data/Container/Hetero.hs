{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.Container.Hetero where

import Prelude
import Data.Monoid
import Data.Container.Class
import Data.Typeable         hiding (cast)
import Unsafe.Coerce         (unsafeCoerce)
import Data.Container.Poly
import Data.Default
import Control.Lens.Utils    hiding (Getter, Setter)
import Data.Functor.Utils
import GHC.Generics    (Generic)
import Control.DeepSeq (NFData)
import Data.Prop


-- === Definitions === --

newtype Hetero a = Hetero a deriving (Generic, NFData, Show, Eq, Ord, Functor, Traversable, Foldable, Default)
makeWrapped ''Hetero


-- === Instances === ---

instance Monoid a => Monoid (Hetero a) where
    mempty = Hetero mempty
    mappend (Hetero a) (Hetero b) = Hetero $ a <> b

-- Properties

type instance Prop p (Hetero a) = Prop p a
instance Getter p a => Getter p (Hetero a) where getter p   = getter p ∘ unwrap'
instance Setter p a => Setter p (Hetero a) where setter p v = wrapped' %~ setter p v



-----------------
-- === Ptr === --
------------------

newtype Ptr  i   a = Ptr i              deriving (Show)
newtype HPtr i m a = HPtr (Ptr i (m a)) deriving (Show)

class PtrFrom p i | p -> i where
    ptrFrom :: p -> Ptr i a

class    PtrIdx p i | p -> i    where ptrIdx :: p -> i
instance PtrIdx (Ptr  i   a) i  where ptrIdx (Ptr i)  = i
instance PtrIdx (HPtr i m a) i  where ptrIdx (HPtr p) = ptrIdx p

instance {-# OVERLAPPABLE #-} (p ~ i) => PtrFrom p         i where ptrFrom = Ptr
instance                                 PtrFrom (Ptr i a) i where ptrFrom (Ptr i) = Ptr i

