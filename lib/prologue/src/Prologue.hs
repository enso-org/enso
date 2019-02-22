{-# LANGUAGE CPP #-}

#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8,6,1,0)
{-# LANGUAGE NoStarIsType #-}
#endif
#endif

module Prologue (module Prologue, module X) where


-- === Orphans === --

import Prologue.OrphanInstances()


-- === Basic === --

import Prelude                         as X ( until, asTypeOf, error, errorWithoutStackTrace, undefined
                                            , seq, ($!)
                                            , map, filter, null, length, (!!), reverse
                                            , scanl, scanl1, scanr, scanr1
                                            , iterate, repeat, cycle
                                            , take, drop, takeWhile, dropWhile, span, break, splitAt
                                            , notElem, lookup
                                            , zip, zip3, zipWith, zipWith3, unzip, unzip3
                                            , lines, words, unwords
                                            , ReadS, Read (readsPrec, readList), reads, readParen, lex
                                            )

-- === Data types === --
import Prologue.Control.DeepSeq         as X
import Prologue.Data.Basic              as X
import Prologue.Data.Default            as X
import Prologue.Data.Default1           as X
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
import Prologue.Text.Show               as X
import Prologue.Text.Show.Styled        as X
import Text.Show.Functions              as X ()
import Data.Void                        as X (Void, absurd)


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
import Data.Functor                     as X (Functor, fmap, (<$), ($>), (<$>))
import Data.Functor.Utils               as X
import Data.Functor.Classes             as X (Eq1, eq1, Ord1, compare1, Read1, readsPrec1, Show1, showsPrec1)
import Data.Functor.Foldable            as X (Fix(Fix))
import Data.Functor.Identity            as X (Identity(Identity), runIdentity)
import Data.String.Class                as X (IsString (fromString), ToString (toString))
import Data.Monoids                     as X
import Prologue.Data.Pointed            as X
import GHC.OverloadedLabels             as X (IsLabel, fromLabel)

-- === Deriving === --
import Text.Show.Deriving               as X (deriveShow1, deriveShow2)
import Text.Read.Deriving               as X (deriveRead1, deriveRead2)
import Data.Eq.Deriving                 as X (deriveEq1, deriveEq2)
import Data.Ord.Deriving                as X (deriveOrd1, deriveOrd2)

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

-- === Containers === --
import Data.Item                        as X
import Data.List.Class                  as X

-- === Exts === --
import GHC.Exts                         as X (lazy, inline, oneShot)

-- === Types === --
import Data.Constraints                 as X (Constraints)
import Data.Type.Equality               as X ((:~:), type(==), TestEquality, testEquality) -- + (~~), (:~~:) after base update
import Prologue.Type.Reflection         as X

-- === Debugging === --
import Debug.Trace                      as X (trace, traceShow)
import GHC.Exts                         as X (breakpoint, breakpointCond)
import GHC.Stack                        as X ( CallStack, HasCallStack, callStack, emptyCallStack, freezeCallStack, getCallStack, popCallStack
                                             , prettyCallStack, pushCallStack, withFrozenCallStack, currentCallStack)
import Type.Error                       as X (TypeError, ErrorMessage(ShowType, (:<>:), (:$$:)), ErrMsg, TypeErrorIf, TypeAssert)
import Prologue.Debug.Placeholders      as X

-- === Quasi Quoters == --
import Prologue.Data.String.QQ          as X (qqStr, qqRawStr, qqTxt)


-- === Typelevel === --
import GHC.TypeLits                     as X (Nat, Symbol, type (-), type (+), type (*), type (^), CmpNat, CmpSymbol) -- someSymbolVal and typelits reify?
import Type.Known                       as X (KnownType, KnownTypeVal, fromType, fromType', KnownNat, KnownSymbol)
import Type.Show                        as X
import Data.Kind                        as X (Type, Constraint)
-- import Type.Operators                   as X
-- import Type.Monoid                      as X (type (<>))
-- import Type.Applicative                 as X (type (<$>), type (<*>))

-- === Unsafe === --
import Unsafe.Coerce                    as X (unsafeCoerce)

-- === Lenses === --
import Control.Lens                     as X (Lens, Lens', Iso, Iso', Traversal, Traversal', At, Ixed, Index, IxValue, ix, iso, at, from, lens, over, both, _1, _2, _3, _4, _5, _6, _7, _8, _9)
import Control.Lens                     as X (view, set)
import Control.Lens                     as X (_1, _2, _3)
import Control.Lens                     as X ((^.), (.~), (%~), (^?), (<&>))
import Control.Lens.Utils.Wrapped       as X (wrap, unwrap, wrapped, wrapped')
import Control.Lens.Utils.TH            as X (makeLenses, makeClassy)

import Control.Lens.At                  as X ()
import Control.Lens.Cons                as X ()
import Control.Lens.Each                as X ()
import Control.Lens.Empty               as X ()
import Control.Lens.Equality            as X ()
import Control.Lens.Fold                as X () -- hiding (pre)
import Control.Lens.Getter              as X ()
import Control.Lens.Indexed             as X ()
import Control.Lens.Iso                 as X () -- hiding (lazy)
import Control.Lens.Lens                as X ()
import Control.Lens.Level               as X ()
import Control.Lens.Prism               as X ()
import Control.Lens.Reified             as X ()
import Control.Lens.Review              as X ()
import Control.Lens.Setter              as X ()
import Control.Lens.TH                  as X () -- hiding (makeLenses, makeClassy) -- Lens.Utils provide better versions
import Control.Lens.Traversal           as X ()
import Control.Lens.Tuple               as X ()
import Control.Lens.Type                as X ()
import Control.Lens.Wrapped             as X (Wrapped, Unwrapped, _Wrapped, _Unwrapped, _Wrapping, _Unwrapping, _Wrapped', _Unwrapped', _Wrapping', _Unwrapping', Rewrapped, ala, alaf)
import Control.Lens.Zoom                as X ()


-- === Concurrency === --

import Control.Concurrent               as X (ThreadId, myThreadId, forkIO, forkFinally, killThread)
