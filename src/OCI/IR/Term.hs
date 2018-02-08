module OCI.IR.Term where

import           Prelude

import           Control.Lens.Utils
import           Data.Coerce               (coerce)
import           Data.Convert              (Convertible, convert)
import           Data.Monoids
import           Data.Word                 (Word8)
import           GHC.Exts                  (IsList, Item, fromList, toList)
import           GHC.Generics              (Generic)
import           Foreign.Ptr               (Ptr, castPtr)
import           Foreign.Storable          (Storable, alignment, peek,
                                            peekByteOff, poke, pokeByteOff,
                                            sizeOf)

import Foreign.Storable.Utils (sizeOf', alignment', castPtrTo, intPtr)


------------------
-- === Edge === --
------------------

newtype EdgeID = EdgeID Int  deriving (Generic, Show, Storable, Eq)
newtype Edge a = Edge EdgeID deriving (Generic, Show, Storable, Eq)



----------------
-- === IR === --
----------------

-- === IR Atoms === ---

newtype Var a = Var { __name :: Int } deriving (Show, Eq)
data    Acc a = Acc { __base :: {-# UNPACK #-} !(Edge a)
                    , __name :: {-# UNPACK #-} !(Edge a)
                    } deriving (Show, Eq)

data UniCore a
    = UVar {-# UNPACK #-} !(Var a)
    | UAcc {-# UNPACK #-} !(Acc a)
    deriving (Generic, Show, Eq)




-- === Instances === --

chunkSize :: Int
chunkSize = sizeOf' @Int

instance Storable a => Storable (UniCore a) where
    sizeOf    _ = 3 * chunkSize ; {-# INLINE sizeOf    #-}
    alignment _ = chunkSize     ; {-# INLINE alignment #-}
    peek ptr = peek (intPtr ptr) >>= \case
        0 -> UVar <$> peekByteOff ptr chunkSize
        1 -> UAcc <$> peekByteOff ptr chunkSize
        _ -> error "Unrecognized constructor"
    {-# INLINE peek #-}
    poke ptr = \case
        UVar !a -> poke (intPtr ptr) 0 >> pokeByteOff ptr chunkSize a
        UAcc !a -> poke (intPtr ptr) 1 >> pokeByteOff ptr chunkSize a
    {-# INLINE poke #-}

instance Storable a => Storable (Acc a) where
    sizeOf    _ = 2 * chunkSize ; {-# INLINE sizeOf    #-}
    alignment _ = chunkSize     ; {-# INLINE alignment #-}
    peek ptr = Acc <$> peek (castPtr ptr) <*> peekByteOff ptr chunkSize ; {-# INLINE peek #-}
    poke ptr = \(Acc !b !n) -> poke (castPtr ptr) b >> pokeByteOff ptr chunkSize n ; {-# INLINE poke #-}

instance Storable a => Storable (Var a) where
    sizeOf    _ = chunkSize ; {-# INLINE sizeOf    #-}
    alignment _ = chunkSize ; {-# INLINE alignment #-}
    peek ptr = Var <$> peek (castPtr ptr)        ; {-# INLINE peek #-}
    poke ptr = \(Var !n) -> poke (castPtr ptr) n ; {-# INLINE poke #-}



---------------------------
-- === Testing utils === --
---------------------------

mkSampleData :: Int -> Int -> UniCore ()
mkSampleData i j = UAcc $ Acc (Edge (EdgeID i)) (Edge (EdgeID j))

fromSampleData :: UniCore () -> Int
fromSampleData (UAcc (Acc (Edge (EdgeID i)) _)) = i



newtype Spec a = Spec a deriving (Show, Functor, Foldable, Traversable)
makeLenses ''Spec


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
