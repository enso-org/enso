{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.Data.Target where

import Prologue

import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Foreign.Storable                   as StdStorable
import qualified Foreign.Storable.Class             as Storable
import qualified Foreign.Storable.Deriving          as Storable
import qualified Luna.IR                            as IR
import qualified Luna.Pass.Attr                     as Attr

data Target
    = Function IR.Qualified IR.Name
    | Method   IR.Qualified IR.Name IR.Name
    | Unknown
    deriving (Show, Eq)
Storable.deriveNoContext ''Target
GTraversable.derive      ''Target

instance Storable.KnownConstantSize Target where
    constantSize = StdStorable.sizeOf (undefined :: Target)

instance MonadIO m => Storable.Peek t m Target
instance MonadIO m => Storable.Poke t m Target

type instance Attr.Type Target = Attr.Atomic
instance Default Target where
    def = Unknown

