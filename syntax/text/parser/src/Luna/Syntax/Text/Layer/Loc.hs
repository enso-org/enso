{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Layer.Loc where

-- import Data.Abstract
-- import Data.Event          hiding (Listener)
-- import Data.Text.Position
-- import Luna.IR.ToRefactor
-- import Luna.IR.ToRefactor2 (Listener, addElemEventListener, listener,
--                             tpElemPass)
-- import OCI.IR.Class
-- import OCI.IR.Layer.Class
-- import OCI.IR.Layer.Req
-- import OCI.Pass.Class
-- import OCI.Pass.Definition
-- import OCI.Pass.Manager
-- import Prologue
-- import Type.Any            (AnyType)


-- -------------------
-- -- === Layer === --
-- -------------------

-- type instance LayerData Range t = Range

-- -- FIXME[WD]: Make abstraction allowing creating layers from scratch in passes. Maybe such layers deserve special place in pass desc?
-- locInit :: Req m '[Writer // Layer // Abstract (Elem t) // Range] => Listener (New // Elem t) m
-- locInit = listener $ \(a,_) -> putLayer @Range a $ error "Core fatal: No Loc layer provided"
-- makePass 'locInit

-- init :: MonadPassManager m => m ()
-- init = addElemEventListener @Range locInitPass
