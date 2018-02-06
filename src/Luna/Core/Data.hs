module Luna.Core.Data where

import           Prelude

import           Control.Lens.Utils
import           Data.Coerce               (coerce)
import           Data.Convert              (Convertible, convert)
import           Data.Monoids
import           Data.Word                 (Word8)
import           GHC.Exts                  (IsList, Item, fromList, toList)
import           GHC.Generics              (Generic)
import           Foreign.Storable.TH
import           Foreign.Ptr               (Ptr, castPtr)
import           Foreign.Storable          (Storable, alignment, peek,
                                            peekByteOff, poke, pokeByteOff,
                                            sizeOf)

newtype Key a = Key Int deriving (Show)

instance Convertible Int (Key a) where convert = coerce ; {-# INLINE convert #-}
instance Convertible (Key a) Int where convert = coerce ; {-# INLINE convert #-}

newtype Keyx     = Keyx Int deriving (Generic, Show, Storable, Eq)

newtype ULink a = ULink Keyx deriving (Generic, Show, Storable, Eq)

newtype Var a = Var { __name :: Int } deriving (Show, Eq)
data    Acc a = Acc { __base :: {-# UNPACK #-} !(ULink a), __name :: {-# UNPACK #-} !(ULink a)} deriving (Show, Eq)

data UniCore a = UVar {-# UNPACK #-} !(Var a)
               | UAcc {-# UNPACK #-} !(Acc a)
               deriving (Generic, Show, Eq)


newtype Spec a = Spec a deriving (Show, Functor, Foldable, Traversable)
makeLenses ''Spec


-- storable instances --

sizeOf'    :: forall a. Storable a => Int
alignment' :: forall a. Storable a => Int
sizeOf'    = sizeOf    (undefined :: a) ; {-# INLINE sizeOf'    #-}
alignment' = alignment (undefined :: a) ; {-# INLINE alignment' #-}

castPtrTo :: forall b a. Ptr a -> Ptr b
castPtrTo = castPtr ; {-# INLINE castPtrTo #-}


-- instances --

instance Storable a => Storable (UniCore a) where
    sizeOf    _ = 2 * sizeOf' @Int + 1 ; {-# INLINE sizeOf #-}
    alignment _ = alignment' @Int ; {-# INLINE alignment #-}
    peek ptr = peekByteOff @Word8 ptr (2 * sizeOf' @Int) >>= \case
        0 -> UVar <$> peek (castPtr ptr)
        1 -> UAcc <$> peek (castPtr ptr)
        _ -> error "Unrecognized constructor"
    {-# INLINE peek #-}
    poke ptr = \case
        UVar !a -> poke (castPtr ptr) a
        UAcc !a -> poke (castPtr ptr) a
    {-# INLINE poke #-}


instance Storable a => Storable (Acc a) where
    sizeOf    _ = 2 * sizeOf' @Int ; {-# INLINE sizeOf #-}
    alignment _ = alignment' @Int ; {-# INLINE alignment #-}
    peek ptr = Acc <$> peek (castPtr ptr) <*> peekByteOff (castPtr ptr) (sizeOf' @Int) ; {-# INLINE peek #-}
    poke ptr = \(Acc !b !n) -> poke (castPtr ptr) b >> pokeByteOff ptr (sizeOf' @Int) n ; {-# INLINE poke #-}

instance Storable a => Storable (Var a) where
    sizeOf    _ = sizeOf' @Int
    alignment _ = alignment' @Int
    peek ptr = Var <$> peek (castPtr ptr)
    poke ptr = \(Var !n) -> poke (castPtr ptr) n


deriveStorable ''Var

instance Storable a => Storable (Spec a) where
    sizeOf    _ = sizeOf' @a + 1                   ; {-# INLINE sizeOf    #-}
    alignment _ = alignment' @a                    ; {-# INLINE alignment #-}
    peek      p = coerce <$> peek @a (castPtr p)   ; {-# INLINE peek      #-}
    poke      p a = poke @a (castPtr p) (coerce a) ; {-# INLINE poke      #-}


-------------------------
-- === Strict List === --
-------------------------
-- NOTE[piotrMocz]: Could alternatively use Data.List.Strict

data List = Cons {-# UNPACK #-} !Int List | Null deriving (Show) -- TODO: Why making strict spine makes it so slow to generate? With lazy one, even if we use all the elements, the whole process is shorter than generating it with strict spine.

instance Mempty    List where mempty = Null ; {-# INLINE mempty  #-}
instance Semigroup List where
    l <> r = case l of
        Null     -> r
        Cons a t -> Cons a (t <> r)
    {-# INLINE (<>) #-}

instance IsList List where
    type Item List = Int
    toList   = \case
        Null      -> []
        Cons a as -> a : toList as
    fromList x = case x of
        (a:as) -> Cons a $ fromList as
        []     -> Null
    {-# INLINE toList   #-}
    {-# INLINE fromList #-}
