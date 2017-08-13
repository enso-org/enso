{-# LANGUAGE NoMonomorphismRestriction #-} -- FIXME: remove

module Prologue2 (module Prologue2, module X) where



-- === Basic === --

import Prelude                         as X ( until, asTypeOf, error, errorWithoutStackTrace, undefined
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

-- === Data types === --
import Prologue.Control.DeepSeq         as X
import Prologue.Data.Basic              as X
import Prologue.Data.Default            as X
import Prologue.Data.Either             as X
import Prologue.Data.Maybe              as X
import Prologue.Data.Num                as X
import Prologue.Data.Show               as X
import Prologue.Data.Tuple              as X
import Data.Function                    as X (id, const, flip, ($), (&), on)
import Data.Text                        as X (Text)
import Data.Data                        as X (Data)
import Data.List.NonEmpty               as X (NonEmpty ((:|)))
import GHC.Generics                     as X (Generic)
import GHC.Enum                         as X ( Enum (succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
                                             , Bounded (minBound, maxBound)
                                             )
import Text.Read                        as X (readPrec) -- new style Read class implementation


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
import Data.Functor                     as X (Functor, fmap, (<$), ($>), (<$>), void)
import Data.Functor.Utils               as X
import Data.Functor.Classes             as X ( Eq1, eq1, Ord1, compare1, Read1, readsPrec1, Show1, showsPrec1
                                             , readsData, readsUnary, readsUnary1, readsBinary1, showsUnary, showsUnary1, showsBinary1 )
import Data.String.Class                as X (IsString (fromString), ToString (toString))
import Data.Monoids                     as X
import Prologue.Data.Pointed            as X

-- === Errors === --
import Control.Exception.Base           as X (assert)
import Control.Monad.Fail               as X (MonadFail, fail)
import Control.Exception                as X (Exception, SomeException, toException, fromException, displayException)
import Control.Monad.Catch              as X ( MonadThrow, throwM
                                             , MonadCatch, catch, catchAll, catchIOError, catchJust, catchIf
                                             , MonadMask, mask, uninterruptibleMask, mask_, uninterruptibleMask_
                                             )
import Data.Impossible                  as X
import Prologue.Control.Error           as X

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
import Type.Error                       as X (TypeError, ErrorMessage(ShowType, (:<>:), (:$$:)), ErrMsg, TypeErrorIf, TypeAssert)
import Prologue.Debug.Placeholders      as X

-- === Quasi Quoters == --
import Prologue.Data.String.QQ          as X (str, rawStr, txt)


-- === Typelevel === --
import GHC.TypeLits                     as X (Nat, Symbol, type (-), type (+), type (*), type (^), CmpNat, CmpSymbol) -- someSymbolVal and typelits reify?
import Type.Known                       as X (KnownType, KnownTypeVal, fromType)
import Type.Show                        as X
import Data.Kind                        as X (Type, Constraint)
import Type.Operators                   as X
import Type.Monoid                      as X (type (<>))
import Type.Applicative                 as X (type (<$>), type (<*>))

-- === Unsafe === --
import Unsafe.Coerce                    as X (unsafeCoerce)

-- === Lenses === --
import Control.Lens.Wrapped             as X (Wrapped, _Wrapped, _Unwrapped, _Wrapping, _Unwrapping, _Wrapped', _Unwrapped', _Wrapping', _Unwrapping', op, ala, alaf)
import Control.Lens.Wrapped.Utils       as X
import Control.Lens.Utils               as X hiding (lazy)
