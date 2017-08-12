{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME: remove

module Prologue2 (module Prologue2, module X) where


-- === Data types === --
import Prologue.Data.Basic              as X
import Prologue.Data.Num                as X
import Prologue.Data.Show               as X
import Prologue.Data.Maybe              as X
import Data.Function                    as X (id, const, flip, ($), (&), on)
import GHC.Generics                     as X (Generic)
import Data.Text                        as X (Text)
import Data.Data                        as X (Data)
import Data.List.NonEmpty               as X (NonEmpty ((:|)))

-- === Monads === --
import Prologue.Control.Monad           as X
import Prologue.Control.Monad.IO        as X
import Prologue.Control.Monad.Primitive as X
import Control.Applicative              as X ( Applicative, pure, (<*>), (*>), (<*), (<$>), (<$), (<**>), liftA, liftA2, liftA3, optional
                                             , Alternative, empty, (<|>), some, many
                                             , ZipList
                                             )
import Control.Monad.Fix                as X (MonadFix, mfix, fix)
import Prologue.Control.Monad.Trans     as X
import Control.Monad.Identity           as X (Identity, runIdentity)
import Control.Monad.Trans.Identity     as X (IdentityT, runIdentityT)
import Control.Comonad                  as X (Comonad, extract, duplicate, extend, (=>=), (=<=), (<<=), (=>>))
import Control.Comonad                  as X (ComonadApply, (<@>), (<@), (@>), (<@@>), liftW2, liftW3)


-- === Basic typeclasses === --
import Prologue.Data.Foldable           as X
import Prologue.Data.Traversable        as X
import Prologue.Data.Bifunctor          as X
import Prologue.Data.Ix                 as X
import Data.Functor.Utils               as X
import Data.String.Class                as X (IsString (fromString), ToString (toString))
import Data.Functor.Classes             as X (Eq1, eq1, Ord1, compare1, Read1, readsPrec1, Show1, showsPrec1) -- FIXME[WD]: Think if we can use (not yet exported) utils to better implement show instead of our fixes
import Data.Monoids                     as X

-- === Errors === --
import Control.Exception.Base           as X (assert)
import Prologue.Control.Error           as X
import Control.Monad.Fail               as X (MonadFail, fail)
import Control.Exception                as X (Exception, SomeException, toException, fromException, displayException)
import Control.Monad.Catch              as X ( MonadThrow, throwM
                                             , MonadCatch, catch, catchAll, catchIOError, catchJust, catchIf
                                             , MonadMask, mask, uninterruptibleMask, mask_, uninterruptibleMask_
                                             )

-- === Conversions === --
import Data.Coerce                      as X (Coercible, coerce)
import Data.Convert                     as X

-- === Exts === --
import GHC.Exts                         as X (lazy, inline) -- + oneShot after base update

-- === Types === --
import Data.Constraints                 as X (Constraints)
import Data.Type.Equality               as X ((:~:), type(==), TestEquality, testEquality) -- + (~~), (:~~:) after base update
import Prologue.Data.Typeable           as X

-- === Debugging === --
import Debug.Trace                      as X (trace, traceShow)
import GHC.Exts                         as X (breakpoint, breakpointCond)
import GHC.Stack                        as X ( CallStack, HasCallStack, callStack, emptyCallStack, freezeCallStack, getCallStack, popCallStack
                                             , prettyCallStack, pushCallStack, withFrozenCallStack, currentCallStack)
import GHC.TypeLits                     as X (TypeError, ErrorMessage(Text, ShowType, (:<>:), (:$$:)))
import Prologue.Debug.Placeholders      as X

-- === Quasi Quoters == --
import Prologue.Data.String.QQ          as X (str, rawStr, txt)


-- === Typelevel === --
import GHC.TypeLits                     as X (Nat, Symbol, type (-), type (+), type (*), type (^), CmpNat, CmpSymbol) -- someSymbolVal and typelits reify?
import Type.Known                       as X (KnownType, KnownTypeVal, fromType)
import Data.Kind                        as X (Type, Constraint)


-- === Unsafe === --
import Unsafe.Coerce                    as X (unsafeCoerce)





-- ////////////////////////////////////////// --
-- To refactor below this line
-- ////////////////////////////////////////// --

import qualified Prelude as Prelude
import Prelude                   as X ( Enum (succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
                                      , Bounded (minBound, maxBound)
                                      , Functor (fmap, (<$)), (<$>)
                                      , until, asTypeOf, error, errorWithoutStackTrace, undefined
                                      , seq, ($!)
                                      , map, filter, head, last, tail, init, null, length, (!!), reverse
                                      , scanl, scanl1, scanr, scanr1
                                      , iterate, repeat, cycle
                                      , take, drop, splitAt, takeWhile, dropWhile, span, break
                                      , notElem, lookup
                                      , zip, zip3, zipWith, zipWith3, unzip, unzip3
                                      , lines, words, unwords
                                      , ReadS, Read (readsPrec, readList), reads, readParen, read, lex
                                      )




import Data.Container.Class       as X (Container, Index, Item)
import Data.Container.List        as X (FromList, fromList, ToList, toList, asList, IsList)
import Data.Impossible            as X
import Data.Tuple.Curry           as X (Curry)
import Data.Tuple.Curry.Total     as X (Uncurried', Curry', curry')
import Type.Operators             as X -- (($), (&))
import Type.Show                  as X (TypeShow, showType, showType', printType, ppPrintType, ppShowType)
import Type.Monoid                as X (type (<>))
import Type.Applicative           as X (type (<$>), type (<*>))
import Type.Error                 as X
import Text.Read                  as X (readPrec) -- new style Read class implementation





-- === Lenses === --
import Control.Lens.Wrapped       as X (Wrapped, _Wrapped, _Unwrapped, _Wrapping, _Unwrapping, _Wrapped', _Unwrapped', _Wrapping', _Unwrapping', op, ala, alaf)
import Control.Lens.Wrapped.Utils as X
import Control.Lens.Utils         as X hiding (lazy)

-- === Data types === --


-- === Either === --
import Control.Monad.Trans.Either as X (EitherT(EitherT), runEitherT, eitherT, hoistEither, left, right, swapEitherT, mapEitherT)
import Data.Either.Combinators    as X (isLeft, isRight, mapLeft, mapRight, whenLeft, whenRight, leftToMaybe, rightToMaybe, swapEither)
import Data.Either                as X (either, partitionEithers)




import Data.Copointed             as X (Copointed, copoint)
import Data.Pointed               as X (Pointed, point)

-- Tuple handling
import Prologue.Data.Tuple        as X

-- Data description
import Prologue.Data.Default      as X

-- Normal Forms
import Prologue.Control.DeepSeq   as X

-- Missing instances
import Data.Default.Instances.Missing ()

import Data.Functor.Compose




-- Placeholders

import qualified Data.List as List
import           Data.List as X (sort)




whenLeft_ :: Monad m => Either a b -> m () -> m ()
whenLeft_ e f = whenLeft e (const f)

whenRight_ :: Monad m => Either a b -> m () -> m ()
whenRight_ e f = whenRight e $ const f

whenRightM :: Monad m => m (Either a b) -> (b -> m ()) -> m ()
whenRightM a f = do
    a' <- a
    whenRight a' f

withRightM :: Monad m => (r -> m (Either l r')) -> Either l r -> m (Either l r')
withRightM f = \case
    Left  l -> return $ Left l
    Right r -> f r






--
-- switchM :: Monad m => m Bool -> a -> a -> m a
-- switchM cond fail ok = do
--   c <- cond
--   return $ if c then ok else fail




show' :: (Show a, IsString s) => a -> s
show' = fromString . Prelude.show

foldlDef :: (a -> a -> a) -> a -> [a] -> a
foldlDef f d = \case
    []     -> d
    (x:xs) -> foldl f x xs



ifElseId :: Bool -> (a -> a) -> (a -> a)
ifElseId cond a = if cond then a else id




evalWhenLeft :: Monad m => m (Either l r) -> m () -> m (Either l r)
evalWhenLeft me f = do
    e <- me
    case e of
        Left  _ -> e <$ f
        Right _ -> return e

evalWhenRight :: Monad m => m (Either l r) -> m () -> m (Either l r)
evalWhenRight me f = do
    e <- me
    case e of
        Left  _ -> e <$ f
        Right _ -> return e


infixl 1 <!>
evalWhenWrong, (<!>) :: (Monad m, CanBeWrong a) => m a -> m () -> m a
(<!>) = evalWhenWrong
evalWhenWrong ma f = do
    a <- ma
    if isWrong a then a <$ f
                 else return a

class CanBeWrong a where
    isWrong :: a -> Bool

instance CanBeWrong (Maybe  a)   where isWrong = isNothing
instance CanBeWrong (Either l r) where isWrong = isLeft




guarded :: Alternative f => Bool -> a -> f a
guarded b a = case b of True  -> pure a
                        False -> empty




composed :: Iso' (f (g a)) (Compose f g a)
composed = iso Compose getCompose



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







partitionMaybeTaggedList :: [(a, Maybe b)] -> ([a], [(a,b)])
partitionMaybeTaggedList = \case
    []             -> ([], [])
    ((a, mb) : ls) -> partitionMaybeTaggedList ls & case mb of
        Nothing -> _1 %~ (a:)
        Just b  -> _2 %~ ((a,b):)



elem' :: Eq a => a -> [a] -> Bool
elem' = elem

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [a] -> Just $ fst a
    _   -> Nothing




tryReads :: forall s' s a. (Read a, Convertible' s String) => s -> Either String a
tryReads s = case reads (convert' s) of
    [(a,[])]  -> Right a
    ((_,s):_) -> Left  s
    _         -> Left "No read"
