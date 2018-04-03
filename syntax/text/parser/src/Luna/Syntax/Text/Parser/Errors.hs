{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Errors where

import Prologue hiding (Span, String, Type, span, (<|))

import Text.Megaparsec.Prim (MonadParsec)
import Type.Any             (AnyType)

-- import Control.Monad.State.Dependent
import Data.Text.Position
-- import Luna.IR
-- import Luna.IR.ToRefactor2           (Listener, addElemEventListener, listener,
--                                       tpElemPass)
import Luna.Syntax.Text.Layer.Loc
-- import OCI.IR                     hiding (IRBuilder, get)
-- import OCI.Pass                   hiding (get)
-- import OCI.Pass.Definition

-- import Data.Container.Mono


-- ---------------------------
-- -- === Invalids attr === --
-- ---------------------------

-- -- === Definition === --

-- newtype Invalids = Invalids [Expr Invalid] deriving (Show, Default, Mempty, Semigroup)
-- makeLenses ''Invalids


-- -- === Utils === --

-- registerInvalid :: Editor Attr Invalids m => Expr Invalid -> m ()
-- registerInvalid = modifyAttr_ @Invalids . (<|)


-- -- === Instances === --

-- type instance Item Invalids = Expr Invalid
-- instance Prependable Invalids where
--     prepend t = wrapped %~ (t:)
