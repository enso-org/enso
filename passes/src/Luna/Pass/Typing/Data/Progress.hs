{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Typing.Data.Progress where

import Prologue

import qualified Luna.Pass.Attr as Attr

newtype Progress = Progress Bool
makeLenses ''Progress

type instance Attr.Type Progress = Attr.Atomic
instance Default Progress where
    def = wrap False

