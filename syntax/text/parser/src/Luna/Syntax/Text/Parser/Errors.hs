{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# EXT      InlineAll                 #-}

module Luna.Syntax.Text.Parser.Errors where

import Prologue hiding (Span, String, Type, span, (<|))

-- import Text.Megaparsec.Prim (MonadParsec)
import Type.Any (AnyType)

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
import qualified Luna.IR        as IR
import qualified Luna.Pass.Attr as Attr


---------------------------
-- === Invalids attr === --
---------------------------

-- === Definition === --

newtype Invalids = Invalids [IR.SomeTerm]
    deriving (Show, Default, Mempty, Semigroup)
type instance Attr.Type Invalids = Attr.Atomic
makeLenses ''Invalids


-- -- === Utils === --

register :: Attr.Editor Invalids m => IR.SomeTerm -> m ()
register t = Attr.modify_ @Invalids $ wrapped %~ (t:) ; {-# INLINE register #-}
