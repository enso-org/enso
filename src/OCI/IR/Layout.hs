{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layout where

import Prologue
import Type.Data.Map
import Type.Data.Maybe
import Data.Tag



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
data Nested a b


-- === Instances === --
type instance Base           (a -< _) = a
type instance Rebase       a (_ -< b) = Nested a b
type instance GetSublayout t (_ -< b) = FromMaybe (DefLayout t) (b !? t)



--------------------------------
-- === Basic Layout Types === --
--------------------------------

data TERM; type Term = LayoutTag TERM
data TYPE; type Type = LayoutTag TYPE
data NAME; type Name = LayoutTag NAME


-- Draft >> [Type := ]
--
-- Draft -:: Draft -# Value -* (Draft -:: )
