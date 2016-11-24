{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Symbol.Class (module Luna.IR.Term.Symbol.Class, module X) where

import qualified Luna.Prelude as P
import           Luna.Prelude hiding (Symbol, String, Integer, Rational, Curry, Data)

import Luna.IR.Term.Atom as X (Atom, String, Integer, Rational, Acc, App, Blank, Cons, Lam, Match, Missing, Native, Star, Unify, Var) -- Types only

import Data.Base                 (Base)
import Data.Construction         (Args)
import Luna.IR.Function (Arg)
import Type.Applicative
import Data.Property
import Data.Phantom
import Luna.IR.Term.Format
import qualified Luna.IR.Term.Layout as Layout
import           Luna.IR.Term.Layout (Layout)

import qualified Old.Luna.Syntax.Term.Expr.Lit  as Lit

import Data.Construction
import           Data.RTuple (List(Null, (:-:)))
import qualified Data.RTuple as List
import           Data.Reprx
import Luna.IR.Repr.Styles  (HeaderOnly)


---------------------
-- === Symbols === --
---------------------

-- === Definitions === --

data family Symbol  atom  layout
type        Symbols atoms layout = Symbol <$> atoms <*> '[layout]

data family UniSymbol a
class     IsUniSymbol t l where
    uniSymbol :: Symbol t l -> UniSymbol l


-- === Symbol isomorphisms === --

type AsSymbol s = Symbol (Access Atom s) (Access Layout s)

type  IsSymbol            s s' = (ToSymbol s, FromSymbol s')
type  IsSymbol'           s    = IsSymbol s s
class FromSymbol          s where fromSymbol          :: AsSymbol s -> s
class UncheckedFromSymbol s where uncheckedFromSymbol :: AsSymbol s -> s
class ToSymbol            s where toSymbol            :: s -> AsSymbol s

instance FromSymbol (Symbol atom layout) where fromSymbol = id ; {-# INLINE fromSymbol #-}
instance ToSymbol   (Symbol atom layout) where toSymbol   = id ; {-# INLINE toSymbol   #-}

symbol :: IsSymbol s s' => Iso s s' (AsSymbol s) (AsSymbol s')
symbol = iso toSymbol fromSymbol ; {-# INLINE symbol #-}

symbol' :: IsSymbol' s => Iso' s (AsSymbol s)
symbol' = symbol ; {-# INLINE symbol' #-}



-- === Selectors === --

data Sym = Sym deriving (Show)

type instance Access   Sym (Symbol atom layout) = Symbol atom layout
instance      Accessor Sym (Symbol atom layout) where
    access = id ; {-# INLINE access #-}


-- === Instances === --

-- Properties

type instance Access Atom          (Symbol atom _     ) = atom
type instance Update Atom   atom   (Symbol _    layout) = (Symbol atom layout)

type instance Access Layout        (Symbol _    layout) = layout
type instance Update Layout layout (Symbol atom _     ) = (Symbol atom layout)

type instance Access Format        (Symbol atom _     ) = Access Format atom

-- instance Phantom atom => Accessor Atom     (Symbol atom layout) where access _ = phantom



-- Repr

instance KnownRepr a => Repr HeaderOnly (Symbol a l) where repr _ = fromString $ typeRepr @a ; {-# INLINE repr #-}
