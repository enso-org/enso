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

--import Flowbox.Prelude       hiding (Indexable, index, Repr, repr)
import Prelude
--import Control.Error.Util    (hush)
import Data.Container.Class
--import Data.Constraint.Void
--import Data.Convert.Errors   (TypeMismatch (TypeMismatch))
import Data.Typeable         hiding (cast)
import Unsafe.Coerce         (unsafeCoerce)
import Data.Container.Poly
--import Data.Reprx

--- === Unified values ===

--data Unified ctx where
--    Unified :: (ctx a, UnifiedEl a) => a -> Unified ctx

--type UnifiedEl a = (Typeable a, Show a)

---- instances

--instance Show (Unified ctx) where
--    show (Unified a) = show a

--instance Typeable a => MaybeConvertible (Unified ctx) TypeMismatch a where
--    tryConvert (Unified u) = if tu == ta then Right $ unsafeCoerce u
--                                         else Left  $ TypeMismatch tu ta
--        where tu = typeOf u
--              ta = typeOf (undefined :: a)

--instance {-# OVERLAPPABLE #-}                         Castable (Unified ctx) a where cast (Unified a) = unsafeCoerce a
--instance {-# OVERLAPPABLE #-} (ctx a, UnifiedEl a) => Castable a (Unified ctx) where cast             = Unified


-- === Ptr ===

newtype Ptr  i   a = Ptr i              deriving (Show)
newtype HPtr i m a = HPtr (Ptr i (m a)) deriving (Show)

class PtrFrom p i | p -> i where
    ptrFrom :: p -> Ptr i a

--class (Container cont (Ptr i a) a, Container cont p a) => IsPtr cont p i a where
--    ptr :: cont -> p -> Ptr i a

---- injective TF
--class    PtrTarget (a :: * -> *) (b :: (* -> *) -> *) c | a b -> c, c -> a b --where
--instance PtrTarget (HPtr i h) a {- = -} (h (a (HPtr i h)))
--instance PtrTarget (Ptr  i)   a {- = -} (a (Ptr i))

---- instances

--instance Repr s i => Repr s (HPtr i m a) where repr (HPtr p) = "HPtr" <+> repr p
--instance Repr s i => Repr s (Ptr i a)    where repr (Ptr i)  = "Ptr " <+> repr i

--type instance IxType (Ptr  i   a) = a
--type instance IxType (HPtr i m a) = a

--instance Convertible i (Ptr i a)    where convert = Ptr
--instance Convertible (Ptr i a) i    where convert = ptrIdx
--instance Convertible i (HPtr i m a) where convert = HPtr . convert
--instance Convertible (HPtr i m a) i where convert = ptrIdx
--instance Convertible (Ptr i (m a)) (HPtr i m a) where convert = HPtr

class    PtrIdx p i | p -> i    where ptrIdx :: p -> i
instance PtrIdx (Ptr  i   a) i  where ptrIdx (Ptr i)  = i
instance PtrIdx (HPtr i m a) i  where ptrIdx (HPtr p) = ptrIdx p

instance {-# OVERLAPPABLE #-} (p ~ i) => PtrFrom p         i where ptrFrom = Ptr
instance                                 PtrFrom (Ptr i a) i where ptrFrom (Ptr i) = Ptr i


----- === Hetero Containers ===

--type Hetero ctx cont = HeteroContainer (cont (Unified ctx))
--type Hetero'    cont = Hetero Void1 cont

--newtype HeteroContainer cont = HeteroContainer { _cont :: cont } deriving (Show, Functor, Foldable, Traversable)

--makeLenses ''HeteroContainer


---- basic instances

--type instance ElementByIx idx (HeteroContainer cont) = IxType idx
--type instance IndexOf     el  (HeteroContainer cont) = Ptr (IndexOf el cont) el

----instance HasContainer (HeteroContainer c) (HeteroContainer c) where
----    container = id

--instance Default cont => Default (HeteroContainer cont) where
--    def = HeteroContainer def

--instance Monoid cont => Monoid (HeteroContainer cont) where
--    mempty = HeteroContainer mempty
--    (HeteroContainer c) `mappend` (HeteroContainer c') = HeteroContainer $ c <> c'

---- container instances

--type HeteroTransCtx idx ctx a cont idx' el = ( ElementOf cont ~ el
--                                             , el ~ Unified ctx
--                                             , ctx a, UnifiedEl a
--                                             , IsoConvertible idx idx'
--                                             )


------class (IndexOf el cont ~ idx, ElementByIx idx cont ~ el, Measurable cont) => Container cont idx el where
------    elems   :: cont -> [el]
------    indexes :: cont -> [idx]

----instance (idx ~ Ptr (IndexOf a cont) a) => Container (HeteroContainer cont) idx a

----instance Measurable (HeteroContainer cont)
------ TODO: TO END ^^^

----instance (HeteroTransCtx idx ctx a cont idx' el, Appendable' cont idx' el)
----      => Appendable' (HeteroContainer cont) idx a where
----    append' a (HeteroContainer cont) = (HeteroContainer cont', convert idx') where
----        (cont', idx') = append' (Unified a :: Unified ctx) cont

----instance (HeteroTransCtx idx ctx a cont idx' el, Prependable cont idx' el)
----      => Prependable (HeteroContainer cont) idx a where
----    prepend' a (HeteroContainer cont) = (HeteroContainer cont', convert idx') where
----        (cont', idx') = prepend' (Unified a :: Unified ctx) cont

----instance (HeteroTransCtx idx ctx a cont idx' el, Updatable cont idx' el)
----      => Updatable (HeteroContainer cont) idx a where
----    update idx a = fmap $ update (convert idx) (Unified a :: Unified ctx)

----instance (HeteroTransCtx idx ctx a cont idx' el, Insertable cont idx' el)
----      => Insertable (HeteroContainer cont) idx a where
----    insert       idx a = fmap $ insert       (convert idx) (Unified a :: Unified ctx)
----    unsafeInsert idx a = fmap $ unsafeInsert (convert idx) (Unified a :: Unified ctx)

----instance (HeteroTransCtx idx ctx a cont idx' el, Indexable cont idx' el, Convertible (Unified ctx) a)
----      => Indexable (HeteroContainer cont) idx a where
----    index       idx (HeteroContainer cont) = convert (index       (convert idx) cont :: Unified ctx)

----instance (HeteroTransCtx idx ctx a cont idx' el, Indexable (Unsafe cont) idx' el, Convertible (Unified ctx) a)
----      => Indexable (Unsafe (HeteroContainer cont)) idx a where
----    index idx (unwrap -> (HeteroContainer cont)) = cast $ (unsafely (index $ convert idx) cont :: Unified ctx)


----instance (Indexable2  opts a idx el) => Indexable2  opts (Resizable s a) idx el where index2 opts idx = index2 opts idx . unwrap
