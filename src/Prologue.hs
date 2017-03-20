
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}

module Prologue (
    module Prologue,
    module X
) where

import qualified Prelude


import Control.Applicative        as X
import Control.Conditional        as X (if', ifM, unless, unlessM, notM, xorM, ToBool, toBool)
import Control.Error.Safe         as X hiding (tryTail, tryInit, tryHead, tryLast, tryMinimum, tryMaximum, tryFoldr1, tryFoldl1, tryFoldl1', tryAt, tryRead, tryAssert, tryJust, tryRight)
import Control.Error.Util         as X (hoistMaybe, hush, hushT, note, isJustT, isNothingT, nothing, just, maybeT, isLeftT, isRightT)
import Control.Exception.Base     as X (assert)
import Control.Lens.Wrapped       as X (Wrapped, _Wrapped, _Unwrapped, _Wrapping, _Unwrapping, _Wrapped', _Unwrapped', _Wrapping', _Unwrapping', op, ala, alaf)
import Control.Lens.Wrapped.Utils as X
import Control.Lens.Utils         as X
import Control.Monad              as X (MonadPlus, mplus, mzero, guard, void, join, (<=<), (>=>), zipWithM, zipWithM_, foldM, foldM_, forever)
import Control.Monad.Base         as X
import Control.Monad.Fix          as X (MonadFix)
import Control.Monad.IO.Class     as X (MonadIO, liftIO)
import Control.Monad.Trans        as X (MonadTrans, lift)
import Control.Monad.Trans.Identity as X (IdentityT, runIdentityT)
import Control.Monad.Primitive    as X (PrimState, PrimMonad, primitive)
import Control.Comonad            as X (Comonad, extract, duplicate, extend, (=>=), (=<=), (<<=), (=>>))
import Control.Comonad            as X (ComonadApply, (<@>), (<@), (@>), (<@@>), liftW2, liftW3)

import Data.Ix                    as X (Ix, range, inRange, rangeSize)
import qualified Data.Ix          as Ix
import Data.Bifunctor             as X (Bifunctor, bimap)
import Data.Bool                  as X (bool)
import Data.List                  as X (intersperse)
import Data.Container.Class       as X (Container, Index, Item)
import Data.Container.List        as X (FromList, fromList, ToList, toList, asList)
import Data.Convert               as X
import Data.Foldable              as X (Foldable, traverse_, foldl', foldrM, foldlM, forM_, mapM_, fold)
import Data.Function              as X (on)
import Data.Functor.Utils         as X
import Data.Impossible            as X
import Data.Layer_OLD             as X
--import Data.Layer_OLD.Cover_OLD           as X
import Data.Maybe                 as X (mapMaybe, catMaybes, fromJust, fromMaybe)
import Data.String.Class          as X (IsString (fromString), ToString (toString))
import Data.String.QQ             as X (s)
import Text.RawString.QQ          as X (r)
import Data.Text                  as X (Text)
import Data.Traversable           as X (mapM)
import Data.Tuple.Curry           as X (Curry)
import Data.Tuple.Curry.Total     as X (Uncurried', Curry', curry')
import Data.Typeable              as X (Typeable, Proxy(Proxy), typeOf, typeRep, TypeRep)
import Data.Typeable.Proxy.Abbr   as X (P, p)
import GHC.Exts                   as X (Constraint)
import GHC.Generics               as X (Generic)
import GHC.TypeLits               as X (Nat, Symbol, SomeNat, SomeSymbol, KnownNat, natVal, type (-), type (+))
import Prelude                    as X hiding (mapM, mapM_, print, putStr, putStrLn, (.), curry, uncurry, break, replicate, Monoid, mempty, mappend, mconcat)
import Text.Show.Pretty           as X (ppShow)
import Type.Operators             as X -- (($), (&))
import Type.Show                  as X (TypeShow, showType, showType', printType, ppPrintType, ppShowType)
import Type.Monoid                as X (type (<>))
import Type.Applicative           as X (type (<$>), type (<*>))
import Type.Error                 as X
import Control.Monad.Catch        as X (MonadMask, MonadCatch, MonadThrow, throwM, catch, mask, uninterruptibleMask, mask_, uninterruptibleMask_, catchAll, catchIOError, catchJust, catchIf)
import Text.Read                  as X (readPrec) -- new style Read class implementation
import Data.Kind                  as X (Type, Constraint, type (★), type (*))
import Data.Constraints           as X (Constraints)
import Data.Int                   as X (Int, Int8, Int16, Int32, Int64)
import Data.Word                  as X (Word, Word8, Word16, Word32, Word64)
import Unsafe.Coerce              as X (unsafeCoerce)
import Prologue.Data.Typeable     as X
import Control.Exception          as X (Exception, SomeException, toException, fromException, displayException)
import Data.Data                  as X (Data)
import Data.Functor.Classes       as X (Eq1, eq1, Ord1, compare1, Read1, readsPrec1, Show1, showsPrec1)
import Data.Either.Combinators    as X (isLeft, isRight, mapLeft, mapRight, whenLeft, whenRight, leftToMaybe, rightToMaybe, swapEither)
import Control.Monad.Trans.Either as X (EitherT, runEitherT, eitherT, hoistEither, left, right, swapEitherT)

import Data.Copointed             as X (Copointed, copoint)
import Data.Pointed               as X (Pointed, point)

-- Tuple handling
import Prologue.Data.Tuple        as X

-- Data description
import Prologue.Data.Default      as X
import Data.Monoids               as X

-- Normal Forms
import Prologue.Control.DeepSeq   as X

-- Missing instances
import Data.Default.Instances.Missing ()

import Data.Functor.Compose

import qualified Data.Traversable                   as Traversable
import Debug.Trace as X (trace, traceShow)

-- Placeholders
import Prologue.Placeholders as X (notImplemented, todo, fixme, placeholder, placeholderNoWarning, PlaceholderException(..))


-- Ix

rangeIndex :: Ix a => (a, a) -> a -> Int
rangeIndex = Ix.index

-- IO

print :: (MonadIO m, Show s) => s -> m ()
print    = liftIO . Prelude.print

printLn :: MonadIO m => m ()
printLn = putStrLn ""

putStr :: MonadIO m => String -> m ()
putStr   = liftIO . Prelude.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . Prelude.putStrLn

pprint :: (MonadIO m, Show s) => s -> m ()
pprint = putStrLn . ppShow

--

infixr 1 <<
(<<) = flip (>>)

replicate :: (Num a, Eq a, Enum a) => a -> t -> [t]
replicate 0 _ = []
replicate i c = c : replicate (pred i) c


swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

fromJustM :: Monad m => Maybe a -> m a
fromJustM Nothing  = fail "Prelude.fromJustM: Nothing"
fromJustM (Just x) = return x


whenLeft' :: (Monad m) => Either a b -> m () -> m ()
whenLeft' e f = whenLeft e (const f)

whenRight' :: (Monad m) => Either a b -> m () -> m ()
whenRight' e f = whenRight e $ const f


($>) :: (Functor f) => a -> f b -> f b
($>) =  fmap . flip const


withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust = forM_


lift2 :: (Monad (t1 m), Monad m, MonadTrans t, MonadTrans t1)
      => m a -> t (t1 m) a
lift2 = lift . lift


lift3 :: (Monad (t1 (t2 m)), Monad (t2 m), Monad m, MonadTrans t, MonadTrans t1, MonadTrans t2)
      => m a -> t (t1 (t2 m)) a
lift3 = lift . lift2


switch :: Monad m => m Bool -> a -> a -> m a
switch cond fail ok = do
  c <- cond
  return $ if c then ok else fail




show' :: (Show a, IsString s) => a -> s
show' = fromString . Prelude.show

foldlDef :: (a -> a -> a) -> a -> [a] -> a
foldlDef f d = \case
    []     -> d
    (x:xs) -> foldl f x xs



ifElseId :: Bool -> (a -> a) -> (a -> a)
ifElseId cond a = if cond then a else id


fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM ma = \case
    Just a  -> return a
    Nothing -> ma

fromBoolMaybe :: a -> Bool -> Maybe a
fromBoolMaybe a b = if b then Just a else Nothing


guarded :: Alternative f => Bool -> a -> f a
guarded b a = case b of True  -> pure a
                        False -> empty
{-# INLINE guarded #-}



-- === Safe operations === --

maybeHead :: [a] -> Maybe a
maybeHead []      = Nothing
maybeHead (a : _) = Just a
{-# INLINE maybeHead #-}

maybeTail :: [a] -> Maybe a
maybeTail []       = Nothing
maybeTail [a]      = Just a
maybeTail (_ : as) = maybeTail as
{-# INLINE maybeTail #-}



-- === MapM === ---

type family Traversables (lst :: [* -> *]) :: Constraint where
    Traversables '[]       = ()
    Traversables (t ': ts) = (Traversable t, Traversables ts)

mapM2 :: (Monad m, Traversables '[t1, t2]) => (a -> m b) -> t1 (t2 a) -> m (t1 (t2 b))
mapM2 = mapM ∘ mapM ; {-# INLINE mapM2 #-}

mapM3 :: (Monad m, Traversables [t1, t2, t3]) => (a -> m b) -> t1 (t2 (t3 a)) -> m (t1 (t2 (t3 b)))
mapM3 = mapM ∘ mapM2 ; {-# INLINE mapM3 #-}

mapM4 :: (Monad m, Traversables [t1, t2, t3, t4]) => (a -> m b) -> t1 (t2 (t3 (t4 a))) -> m (t1 (t2 (t3 (t4 b))))
mapM4 = mapM ∘ mapM3 ; {-# INLINE mapM4 #-}

mapM5 :: (Monad m, Traversables [t1, t2, t3, t4, t5]) => (a -> m b) -> t1 (t2 (t3 (t4 (t5 a)))) -> m (t1 (t2 (t3 (t4 (t5 b)))))
mapM5 = mapM ∘ mapM4 ; {-# INLINE mapM5 #-}


composed :: Iso' (f (g a)) (Compose f g a)
composed = iso Compose getCompose


-- Monads

(>>~) :: Monad m => m a -> (a -> m b) -> m a
f >>~ g = do
    fa <- f
    g fa
    return fa

infixr 1 =<<&
(=<<&) :: MonadFix m => (a -> m b) -> m a -> m a
g =<<& f = mdo
    g fa
    fa <- f
    return fa


infixr 1 <=<<, >>=>

(>>=>) :: Monad m => (a -> b -> m c) -> (c -> m d) -> (a -> b -> m d)
f >>=> g = \x y -> f x y >>= g ; {-# INLINE (>>=>) #-}

(<=<<) :: Monad m => (c -> m d) -> (a -> b -> m c) -> (a -> b -> m d)
(<=<<) = flip (>>=>) ; {-# INLINE (<=<<) #-}


-- This is just a garbage-util for dummy Prelude show implementation
-- For more information look here: https://hackage.haskell.org/package/base-4.9.0.0/docs/Text-Show.html
app_prec :: Int
app_prec = 10

showsPrec' = showsPrec (succ app_prec)
showParen' d = showParen (d > app_prec)


-- === MonadTrans === --

type MonadTransInvariants  t m = (Monad m, Monad (t m), MonadTrans t)
type MonadTransInvariants' t m = (Monad m, Monad (t m), MonadTrans t, PrimState m ~ PrimState (t m))

type EqPrims m n = (PrimState m ~ PrimState n)



copointed :: (Pointed t, Copointed t) => Iso (t a) (t b) a b
copointed = iso copoint point

pointed :: (Pointed t, Copointed t) => Iso a b (t a) (t b)
pointed = from copointed

copointed' :: (Copointed t, Functor t) => Lens (t a) (t b) a b
copointed' = lens copoint (\ta b -> fmap (const b) ta)




if_ :: (ToBool cond, Mempty a) => cond -> a -> a
if_ p s = if toBool p then s else mempty

when   :: (Applicative f, ToBool cond)           =>   cond -> f a -> f ()
whenM  :: (Monad m      , ToBool cond)           => m cond -> m a -> m ()
when'  :: (Applicative f, ToBool cond, Mempty a) =>   cond -> f a -> f a
whenM' :: (Monad m      , ToBool cond, Mempty a) => m cond -> m a -> m a
when   p s = if toBool  p then void s else pure ()
when'  p s = if toBool  p then s      else pure mempty
whenM  p s = flip when  s =<< p
whenM' p s = flip when' s =<< p
