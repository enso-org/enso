{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Layout.ENT where

import Luna.Prelude hiding (Simple, String)

import Luna.IR.Internal.IR (AnyExpr)
import Luna.IR.Expr.Layout.Class
import Luna.IR.Expr.Layout.Nested
import Luna.IR.Expr.Format
import Luna.IR.Expr.Atom (Atom, String, Star, Atomic)
import qualified Luna.IR.Expr.Atom as Atom
import Luna.IR.Layer.Type (Type)
import Data.RTuple        (Assoc ((:=)))
import Type.Bool




-----------------
-- === Ent === --
-----------------

-- === Definition === ---

data Ent

type ENT e n t = Layout '[AnyExpr := e, NAME := n, Type := t]
type ET  e   t = Layout '[AnyExpr := e,            Type := t]
type EN  e n   = Layout '[AnyExpr := e, NAME := n           ]
type NT    n t = Layout '[           NAME := n, Type := t]
type E   e     = Layout '[AnyExpr := e                      ]
type N     n   = Layout '[           NAME := n           ]
type T       t = Layout '[                      Type := t]

infixr 7 #>
infixr 7 :>
type a #> n = a >>> N n
type a :> t = a >>> T t

infixr 7 >>
type a >> b = a >>> E b

-------------------------
-- === Cons layout === --
-------------------------

type String' = String :> Cons'

type ConsType t = Atom.Cons >>> NT String' t

type    Cons'  = Cons Star
newtype Cons t = Cons (ConsType t)






type instance Generalizable (Cons t) (Cons t') = Generalizable t t'



type instance Merge (h >>> l) (h' >>> l') = Merge h h' >>> Merge l l'

type instance Merge (Cons t) (Cons t') = Cons (Merge t t')

type instance Merge (Cons t) (Atomic a) = Merge (ConsType t) (Atomic a)

type instance DefaultLayout Type    = Draft -- This instance works fine when the layout is Draft or Atomic. We need to figure out how to handle Thunks, Values and other specific layouts
type instance DefaultLayout NAME    = String
type instance DefaultLayout AnyExpr = Draft


type instance Merge (h >>> l) (Atomic a) = Merge h (Atomic a) >>> Merge l (Atomic a)

type instance Merge (Layout ls) (Atomic a) = Merge (Layout ls) (E (Atomic a))


type instance Merge () (h >>> l) = h >>> l
type instance Merge (h >>> l) () = h >>> l




--- Key Sets

type instance AddKey '[a] a = '[a]

type instance AddKey '[AnyExpr] NAME = '[AnyExpr, NAME]
type instance AddKey '[AnyExpr] Type = '[AnyExpr, Type]

type instance AddKey '[NAME] AnyExpr = '[AnyExpr, NAME]
type instance AddKey '[NAME] Type = '[NAME, Type]

type instance AddKey '[Type] NAME = '[NAME, Type]
type instance AddKey '[Type] AnyExpr = '[AnyExpr, Type]



type instance AddKey '[a,b] a = '[a,b]
type instance AddKey '[a,b] b = '[a,b]
type instance AddKey '[NAME, Type] AnyExpr = '[AnyExpr, NAME, Type]
type instance AddKey '[AnyExpr, Type] NAME = '[AnyExpr, NAME, Type]
type instance AddKey '[AnyExpr, NAME] Type = '[AnyExpr, NAME, Type]

type instance AddKey '[a,b,c] a = '[a,b,c]
type instance AddKey '[a,b,c] b = '[a,b,c]
type instance AddKey '[a,b,c] c = '[a,b,c]

-- FIXME[WD]: change the definition of sets to comparable ones:

-- type instance AnyExpr > NAME = 'False
-- type instance NAME > AnyExpr = 'True
--
-- type instance AnyExpr > Type = 'False
-- type instance Type > AnyExpr = 'True

-- with auto eq:
-- NAME `gt` Expr
-- Type `gt` Expr
-- Type `gt` NAME


type instance Sub t (Form   f) = If (t == AnyExpr) (Form   f) (DefaultLayout t)
type instance Sub t (Atomic a) = DefaultLayout t


type instance Generalizable (h >>> l)  Bottom = 'True
type instance Generalizable (Form   f) Bottom = 'True
type instance Generalizable (Atomic a) Bottom = 'True
