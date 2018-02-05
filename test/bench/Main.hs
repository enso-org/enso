{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans -fno-warn-unused-binds #-}
-- {-# LANGUAGE Strict #-}

module Main where

import Prelude as P hiding (mempty, length)
import Data.Monoids
import Criterion.Main
-- import Control.Monad.State.Layered hiding ((.))
-- import qualified Control.Monad.State.Strict as S
-- import Control.Monad.Codensity
-- import qualified Control.Monad.State.CPS as CPS
-- import Data.Word
-- import Control.Monad.Identity
import GHC.IO          as X (evaluate)
import Control.DeepSeq
-- import System.TimeIt
-- import System.Environment (getArgs)
import System.IO (stdout, hSetBuffering, BufferMode(..))
-- import Control.Monad.Trans.Either
import Control.Monad.ST
import Control.Lens.Utils

import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Storable as Vector hiding (length)
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable.Mutable as Storable
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Vector.Storable.Mutable (MVector)
import FastString (FastString)
import qualified FastString as FastString
import qualified Data.Set as Set
import Data.Set (Set)
import GHC.Generics (Generic)
import Foreign.Storable
import Foreign.CStorable
import Unsafe.Coerce (unsafeCoerce)
import Control.Exception.Base (assert)
import GHC.Exts (IsList, Item, fromList, toList)
import Control.Monad.Primitive (PrimState, PrimMonad)
import Data.Convert
import qualified Control.Monad.IO.Class as IO

import           Data.Typeable             (Typeable, typeOf)
import qualified Data.Vector.Storable      as VS
import           Data.Word                 (Word8)
import           Foreign.Ptr               (Ptr, castPtr)
import           Foreign.Storable          (Storable, alignment, peek,
                                            peekByteOff, poke, pokeByteOff,
                                            sizeOf)
import Data.Coerce
import GHC.STRef (STRef, readSTRef, writeSTRef)

import Data.Functor.Utils
import Control.Monad.Primitive

import Foreign.Storable.TH



coerceTo :: forall b a. Coercible a b => a -> b
coerceTo = coerce ; {-# INLINE coerceTo #-}

coerceVectorTo :: forall b a s. MVector s a -> MVector s b
coerceVectorTo = coerce ; {-# INLINE coerceVectorTo #-}


-------------------------
-- === STRef utils === --
-------------------------

-- === Definitions === --

type STRef' m = STRef (PrimState m)


-- === Utils === --

modifySTRefM :: PrimMonad m => STRef' m t -> (t -> m (t, a)) -> m a
modifySTRefM ref f = do
    (!t, a) <- f =<< liftPrim (readSTRef ref)
    a <$ liftPrim (writeSTRef ref t)
{-# INLINE modifySTRefM #-}

modifySTRefM_ :: PrimMonad m => STRef' m t -> (t -> m t) -> m ()
modifySTRefM_ ref f = modifySTRefM ref (fmap2 (,()) f) ; {-# INLINE modifySTRefM_ #-}

modifySTRef_ :: PrimMonad m => STRef' m t -> (t -> t) -> m ()
modifySTRef_ ref f = modifySTRefM_ ref (return . f) ; {-# INLINE modifySTRef_ #-}





----------------------------
-- === Storable utils === --
----------------------------

sizeOf'    :: forall a. Storable a => Int
alignment' :: forall a. Storable a => Int
sizeOf'    = sizeOf    (undefined :: a) ; {-# INLINE sizeOf'    #-}
alignment' = alignment (undefined :: a) ; {-# INLINE alignment' #-}

castPtrTo :: forall b a. Ptr a -> Ptr b
castPtrTo = castPtr ; {-# INLINE castPtrTo #-}




-------------------------
-- === Strict List === --
-------------------------

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



-----------------
-- === Key === --
-----------------

-- === Definition === --

newtype Key a = Key Int deriving (Show)


-- === Instances ===

instance Convertible Int (Key a) where convert = coerce ; {-# INLINE convert #-}
instance Convertible (Key a) Int where convert = coerce ; {-# INLINE convert #-}





newtype Keyx     = Keyx Int deriving (Generic, Show, Storable, Eq)

newtype ULink a = ULink Keyx deriving (Generic, Show, Storable, Eq)

newtype Var a = Var { __name :: Int } deriving (Show, Eq)
data    Acc a = Acc { __base :: {-# UNPACK #-} !(ULink a), __name :: {-# UNPACK #-} !(ULink a)} deriving (Show, Eq)

data UniCore a = UVar {-# UNPACK #-} !(Var a)
               | UAcc {-# UNPACK #-} !(Acc a)
               deriving (Generic, Show, Eq)


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


newtype Spec a = Spec a deriving (Show, Functor, Foldable, Traversable)
makeLenses ''Spec

instance Storable a => Storable (Spec a) where
    sizeOf    _ = sizeOf' @a + 1                   ; {-# INLINE sizeOf    #-}
    alignment _ = alignment' @a                    ; {-# INLINE alignment #-}
    peek      p = coerce <$> peek @a (castPtr p)   ; {-# INLINE peek      #-}
    poke      p a = poke @a (castPtr p) (coerce a) ; {-# INLINE poke      #-}





-------------------
-- === Store === --
-------------------

-- === Definition === --

type StoreM' m   = StoreM (PrimState m)
data StoreM  s a = StoreM { _vector   :: {-# UNPACK #-} !(MVector s a)
                          , _freeIdxs ::                !List
                          } deriving (Generic)
makeLenses ''StoreM


-- === Info === --

-- | O(1)
length :: Storable a => StoreM s a -> Int
length s = Vector.length $ s ^. vector ; {-# INLINE length #-}


-- === Construction === --

new   :: (PrimMonad m, Storable a) =>        m (StoreM' m a)
alloc :: (PrimMonad m, Storable a) => Int -> m (StoreM' m a)
new      = alloc 1024                                          ; {-# INLINE new   #-}
alloc !i = StoreM <$> Vector.unsafeNew i <*> pure [0 .. i - 1] ; {-# INLINE alloc #-}


-- === Direct modifications === --

-- | O(1)
releaseKey :: Convertible' k Int => k -> StoreM s a -> StoreM s a
releaseKey !k !s = s & freeIdxs %~ (Cons $ convert' k) ; {-# INLINE releaseKey #-}

-- | O(1) / ?
reserveKey :: (PrimMonad m, Storable a, Convertible' Int k) => StoreM' m a -> m (StoreM' m a, k)
reserveKey s = go s (unsafeDoubleGrow s >>= flip go (error "impossible")) where
    go t e = case t ^. freeIdxs of
        Cons i is -> return (s & freeIdxs .~ is, convert' i)
        Null      -> e
    {-# INLINE go #-}
{-# INLINE reserveKey #-}

unsafeGrow :: (PrimMonad m, Storable a) => StoreM' m a -> Int -> m (StoreM' m a)
unsafeGrow !s !i = assert (i > 0) $ vector (flip Vector.unsafeGrow i) s ; {-# INLINE unsafeGrow #-} -- FIXME: alokacja pustych adresow!

unsafeDoubleGrow :: (PrimMonad m, Storable a) => StoreM' m a -> m (StoreM' m a)
unsafeDoubleGrow !s = unsafeGrow s (length s) ; {-# INLINE unsafeDoubleGrow #-}

unsafeWrite :: (PrimMonad m, Storable a, Convertible' k Int) => StoreM' m a -> k -> a -> m ()
unsafeWrite !s !k !a = Vector.unsafeWrite (s ^. vector) (convert' k) a ; {-# INLINE unsafeWrite #-}

unsafeWriteSpec :: forall t a k m. (PrimMonad m, Storable t, Convertible' k Int) => StoreM' m a -> k -> t -> m ()
unsafeWriteSpec !s !k !t = Vector.unsafeWrite (coerceVectorTo @(Spec t) $ s ^. vector) (convert' k) (Spec t) ; {-# INLINE unsafeWriteSpec #-}



-------------------
-- === Tests === --
-------------------

mknodes :: Int -> Vector (UniCore ()) -> IO (Vector (UniCore ()))
mknodes !i !v = do
    nodes <- Vector.unsafeThaw v
    let go 0 = return ()
        go j = Vector.unsafeWrite nodes j (UAcc $ Acc (ULink (Keyx j)) (ULink (Keyx (j + 1)))) >> go (j - 1)
    go i
    Vector.unsafeFreeze nodes
{-# INLINE mknodes #-}

mknodes2 :: Int -> StoreM' IO (UniCore ()) -> IO ()
mknodes2 !i !s = do
    let go 0 _  = return ()
        go j !v = do
            (v', (k :: Int)) <- reserveKey v
            unsafeWriteSpec v k (Acc (ULink (Keyx j)) (ULink (Keyx (j + 1))) :: Acc ())
            go (j - 1) v
    go i s
    return ()
{-# INLINE mknodes2 #-}


mkVec :: Int -> IO (Vector (UniCore ()))
mkVec !i = Vector.unsafeFreeze =<< Vector.new (i + 1)
{-# INLINE mkVec #-}

main :: IO ()
main = do
    vx <- alloc (10^(8::Int) + 1) -- FIXME: it should be done in env

    defaultMain [
          bgroup "mknodes"  $ (\(i :: Int) -> env (mkVec (10 ^ i)) $ \v -> bench ("10e" <> show i) $ nfIO (mknodes  (10 ^ i) v))  <$> [7..8]
        , bgroup "mknodes2" $ (\(i :: Int) -> env (return ())      $ \v -> bench ("10e" <> show i) $ nfIO (mknodes2 (10 ^ i) vx)) <$> [7..8]
        ]

    let v :: Vector (UniCore ())
        v = runST $ do
            nodes <- Vector.new 10
            Vector.write nodes 0 (UAcc $ Acc (ULink (Keyx 1)) (ULink (Keyx 2)))
            Vector.write nodes 1 (UAcc $ Acc (ULink (Keyx 17)) (ULink (Keyx 27)))
            Vector.write nodes 2 (UAcc $ Acc (ULink (Keyx 15)) (ULink (Keyx 25)))
            Vector.freeze nodes
    print $ Vector.unsafeIndex v 0
    print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 0
    print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 1
    print $ Vector.unsafeIndex (unsafeCoerce v :: Vector (Spec (Acc ()))) 2
    print $ alignment' @Int
    print $ alignment' @Char
    print $ alignment' @Bool
