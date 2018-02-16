{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeInType           #-}

module OCI.IR.Layout where

import Prologue hiding (Type)
import qualified Prologue as P
-- import Type.Data.Map
import Type.Data.Property hiding (Set)
import Type.Data.Maybe
import Data.Tag
import Type.Data.Ord

-- import Type.Data.Set.Proxy (Set)
import Type.Data.Set (Set)
import qualified Type.Data.Set as Set



--------------------
-- === Layout === --
--------------------

-- === Definition === --

data LAYOUT
type LayoutTag = Tag LAYOUT


-- === Modification === --

type family Base         layout
type family Rebase       base   layout
type family GetSublayout tp     layout
type family DefLayout    tp



---------------------------
-- === Nested Layout === --
---------------------------

-- === Definition === --

infixr 7 -<
type a -< b = Nested a b
data Nested (a :: P.Type) (b :: Set P.Type)


-- === Instances === --
type instance Base           (a -< _) = a
type instance Rebase       a (_ -< b) = Nested a b
type instance GetSublayout t (_ -< b) = FromMaybe (DefLayout t) (b !? t)



--------------------------
-- === Associations === --
--------------------------

data k := v
type instance Cmp (k := _) (k' := _) = Cmp k k'



--------------------------------
-- === Basic Layout Types === --
--------------------------------

-- === Definition === --

data TERM; type Term = LayoutTag TERM
data TYPE; type Type = LayoutTag TYPE
data NAME; type Name = LayoutTag NAME

defOrder [''Name, ''Type, ''Term]

-- === Utils === --

type family SetSublayout k v l where
    SetSublayout k v (a -< b) = a -< (Set.Insert (k := v) b)
    SetSublayout k v a        = a -< (Set.Singleton (k := v))

type l -*  t = SetSublayout Term t l
type l -:: t = SetSublayout Type t l
type l -#  t = SetSublayout Name t l


-------------------------
-- === Conversions === --
-------------------------

-- type family Coerce
