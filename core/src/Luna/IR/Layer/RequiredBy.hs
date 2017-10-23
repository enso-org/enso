{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.IR.Layer.RequiredBy where

import Luna.Prelude
import OCI.IR.Layer.Class
import OCI.IR.Class        (SubLink, AnyExpr, Import)
import OCI.Pass
import OCI.IR
import OCI.Pass.Definition (makePass)
import Luna.IR.ToRefactor2 (Listener, listener, addExprEventListener, tpElemPass)
import Type.Any (AnyType)
import Data.TypeDesc (getTypeDesc_, typeDesc)
import Luna.IR.Layer.Errors (ErrorSource, ModuleTagged)

data RequiredBy = RequiredBy deriving (Show)

type instance LayerData RequiredBy a = [ModuleTagged ErrorSource]

initRequiredBy :: Req m '[Writer // Layer // AnyExpr // RequiredBy] => Listener (New // Expr l) m
initRequiredBy = listener $ \(t, _) -> putLayer @RequiredBy t []
makePass 'initRequiredBy

importRequiredBy :: Req m '[Writer // Layer // AnyExpr // RequiredBy] => Listener (Import // Expr l) m
importRequiredBy = listener $ \(t, _, l) -> when (getTypeDesc_ @RequiredBy ^. from typeDesc `elem` l) $ putLayer @RequiredBy t []
makePass 'importRequiredBy

init :: MonadPassManager m => m ()
init = addExprEventListener @RequiredBy initRequiredByPass
