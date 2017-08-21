{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.IR.Layer.Errors where

import Luna.Prelude
import OCI.IR.Layer.Class
import OCI.IR.Class        (SubLink, AnyExpr, Import)
import OCI.Pass
import OCI.IR
import OCI.Pass.Definition (makePass)
import Luna.IR.ToRefactor2 (Listener, listener, addExprEventListener, tpElemPass)
import Type.Any (AnyType)
import Data.TypeDesc (getTypeDesc_, typeDesc)

data Errors = Errors deriving (Show)

data ErrorSource  = FromMethod Name Name | FromFunction Name deriving (Show, Eq)
data CompileError = CompileError { _description :: Text
                                 , _arisingFrom :: [ErrorSource]
                                 } deriving (Show, Eq)
type instance LayerData Errors a = [CompileError]

initErrors :: Req m '[Writer // Layer // AnyExpr // Errors] => Listener (New // Expr l) m
initErrors = listener $ \(t, _) -> putLayer @Errors t []
makePass 'initErrors

importErrors :: Req m '[Writer // Layer // AnyExpr // Errors] => Listener (Import // Expr l) m
importErrors = listener $ \(t, _, l) -> when (getTypeDesc_ @Errors ^. from typeDesc `elem` l) $ putLayer @Errors t []
makePass 'importErrors

init :: MonadPassManager m => m ()
init = addExprEventListener @Errors initErrorsPass
