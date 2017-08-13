{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Errors where

import Prologue_old hiding (String, Type, Span, span, (<|))

import Text.Megaparsec.Prim (MonadParsec)
import Type.Any (AnyType)

import OCI.IR hiding (IRBuilder, get)
import Luna.Syntax.Text.Layer.Loc
import OCI.Pass hiding (get)
import OCI.Pass.Definition
import Luna.IR
import Data.Text.Position
import Control.Monad.State.Dependent
import Luna.IR.ToRefactor2 (Listener, listener, tpElemPass, addElemEventListener)

import Data.Container.Mono


---------------------------
-- === Invalids attr === --
---------------------------

-- === Definition === --

newtype Invalids = Invalids [Expr Invalid] deriving (Show, Default, Mempty, Semigroup)
makeLenses ''Invalids


-- === Utils === --

registerInvalid :: Editor Attr Invalids m => Expr Invalid -> m ()
registerInvalid = modifyAttr_ @Invalids . (<|)


-- === Instances === --

type instance Item Invalids = Expr Invalid
instance Prependable Invalids where
    prepend t = wrapped %~ (t:)
