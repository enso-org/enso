{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layout.Typed where

import Luna.Prelude hiding (Simple, String)

import OCI.IR.Class (AnyExpr)
import OCI.IR.Layout.Class
import OCI.IR.Layout.Nested
import OCI.IR.Layout.Format
import Luna.IR.Format (Draft)
import OCI.IR.Term  (TermicType)
import Luna.IR.Layer.Type (Type)
import Data.RTuple        (Assoc ((:=)))
import Type.Bool

import Data.Property (Access)



--------------------------
-- === Typed layout === --
--------------------------

-- === Definition === ---

type ET e t = Layout '[AnyExpr := e, Type := t]
type E  e   = Layout '[AnyExpr := e           ]
type T    t = Layout '[              Type := t]


-- === Operators === --

infixr 7 :>
infixr 7 >>
type a :> t = a >>> T t
type a >> e = a >>> E e


-- === Instances === --

type ToTyped a = a >>> ET (a ^ AnyExpr) (a ^ Type)


type instance Merge (h >>> l) (h' >>> l') = Merge h h' >>> Merge l l'


type instance DefaultLayout Type    = Draft
type instance DefaultLayout AnyExpr = Draft


type instance Merge (h >>> l)  (TermicType a) = Merge (h >>> l) (ToTyped (TermicType a))
type instance Merge (TermicType a) (h >>> l)  = Merge (ToTyped (TermicType a)) (h >>> l)

type instance Merge (h >>> l) (Form a)  = Merge (h >>> l) (ToTyped (Form a))
type instance Merge (Form a)  (h >>> l) = Merge (ToTyped (Form a)) (h >>> l)

type instance Merge (Layout ls) (TermicType a) = Merge (Layout ls) (E (TermicType a))


type instance Merge () (h >>> l) = h >>> l
type instance Merge (h >>> l) () = h >>> l


type instance Generalizable (a >>> sa) (Form b)       = Generalizable (a >>> sa) (ToTyped $ Form b)
type instance Generalizable (a >>> sa) (TermicType b) = Generalizable (a >>> sa) (ToTyped $ TermicType b)


--- Key Sets

type instance AddKey '[a] a = '[a]

type instance AddKey '[AnyExpr] Type = '[AnyExpr, Type]
type instance AddKey '[Type] AnyExpr = '[AnyExpr, Type]



type instance AddKey '[a,b] a = '[a,b]
type instance AddKey '[a,b] b = '[a,b]


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
type instance Sub t (TermicType a) = DefaultLayout t


type instance Generalizable (h >>> l)  Bottom = 'True
type instance Generalizable (Form   f) Bottom = 'True
type instance Generalizable (TermicType a) Bottom = 'True



---- !!! WARNING: testing:

type instance Access AnyExpr (a >> b) = Access AnyExpr a
