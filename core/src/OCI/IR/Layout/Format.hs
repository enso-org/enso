{-# LANGUAGE UndecidableInstances #-}


module OCI.IR.Layout.Format where

import Luna.Prelude

import Type.List      (TakeUntil)
import Type.Container (Every)



---------------------
-- === Formats === --
---------------------

-- === Definitions === --

data Format
data Form a

type SubFormats a = TakeUntil a (Every Format)


-- === Selectors === --

data SuperFormats = SuperFormats deriving (Show)


-- === Instances === --

type instance TypeRepr (Form a) = TypeRepr a
