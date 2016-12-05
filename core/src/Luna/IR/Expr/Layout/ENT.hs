{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr.Layout.ENT where

import Luna.Prelude hiding (Simple, String)

import Luna.IR.Internal.IR (EXPR)
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

type ENT e n t = Layout '[EXPR := e, NAME := n, Type := t]
type ET  e   t = Layout '[EXPR := e,            Type := t]
type EN  e n   = Layout '[EXPR := e, NAME := n           ]
type NT    n t = Layout '[           NAME := n, Type := t]
type E   e     = Layout '[EXPR := e                      ]
type N     n   = Layout '[           NAME := n           ]
type T       t = Layout '[                      Type := t]

infixr 7 #>
infixr 7 :>
type a #> n = a >> N n
type a :> t = a >> T t


-------------------------
-- === Cons layout === --
-------------------------

type String' = String :> Cons'

type ConsType t = Atom.Cons >> NT String' t

type    Cons'  = Cons Star
newtype Cons t = Cons (ConsType t)


type instance Generalizable (Cons t) (Cons t') = Generalizable t t'



type instance Merge (h >> l) (h' >> l') = Merge h h' >> Merge l l'

type instance Merge (Cons t) (Cons t') = Cons (Merge t t')

type instance Merge (Cons t) (Atomic a) = Merge (ConsType t) (Atomic a)

type instance DefaultLayout Type = Star
type instance DefaultLayout NAME = String'
type instance DefaultLayout EXPR = () -- If it was not mentioned explicitly, it was simply absent.


type instance Merge (h >> l) (Atomic a) = Merge h (Atomic a) >> Merge l (Atomic a)

type instance Merge (Layout ls) (Atomic a) = Merge (Layout ls) (E (Atomic a))


type instance Merge () (h >> l) = h >> l
type instance Merge (h >> l) () = h >> l




--- Key Sets

type instance AddKey '[a] a = '[a]

type instance AddKey '[EXPR] NAME = '[EXPR, NAME]
type instance AddKey '[EXPR] Type = '[EXPR, Type]

type instance AddKey '[NAME] EXPR = '[EXPR, NAME]
type instance AddKey '[NAME] Type = '[NAME, Type]

type instance AddKey '[Type] NAME = '[NAME, Type]
type instance AddKey '[Type] EXPR = '[EXPR, Type]



type instance AddKey '[a,b] a = '[a,b]
type instance AddKey '[a,b] b = '[a,b]
type instance AddKey '[NAME, Type] EXPR = '[EXPR, NAME, Type]
type instance AddKey '[EXPR, Type] NAME = '[EXPR, NAME, Type]
type instance AddKey '[EXPR, NAME] Type = '[EXPR, NAME, Type]

type instance AddKey '[a,b,c] a = '[a,b,c]
type instance AddKey '[a,b,c] b = '[a,b,c]
type instance AddKey '[a,b,c] c = '[a,b,c]

-- FIXME[WD]: change the definition of sets to comparable ones:

-- type instance EXPR > NAME = 'False
-- type instance NAME > EXPR = 'True
--
-- type instance EXPR > Type = 'False
-- type instance Type > EXPR = 'True

-- with auto eq:
-- NAME `gt` Expr
-- Type `gt` Expr
-- Type `gt` NAME


type instance Sub t (Form   f) = If (t == EXPR) (Form   f) (DefaultLayout t)
type instance Sub t (Atomic a) = If (t == EXPR) (Atomic a) (DefaultLayout t)
