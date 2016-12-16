module Luna.Pass.Utils.Utils where

import           Luna.Prelude
import qualified Luna.Pass    as Pass
import           Luna.IR
import           Data.TypeVal
import qualified Data.Set     as Set

-- TODO: This is certainly a wrong place for operations like these. Waiting for WD input on where to put them

narrowAtom :: forall a m. (IRMonad m, KnownType (AtomOf a), Readable (ExprLayer Model) m)
           => AnyExpr -> m (Maybe (Expr (AtomOf a)))
narrowAtom expr = do
    exprAtomRep <- getAtomRep expr
    return $ if exprAtomRep == atomRep' @a
      then Just $ unsafeGeneralize expr
      else Nothing

deleteSubtree :: forall l m. (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet, ExprLayer Succs, ExprLayer Type, ExprLayer Model, ExprLinkLayer Model], Emitters m '[DELETE // LINK' EXPR, DELETE // EXPR])
                 => AnyExpr -> m ()
deleteSubtree expr = do
    succs     <- readLayer @Succs expr
    removable <- case Set.toList succs of
        []  -> return True
        [l] -> uncurry (==) <$> readLayer @Model l -- Detect a loop, occuring e.g. with Stars
        _   -> return False
    if removable then do
        inps       <- symbolFields expr
        tp         <- readLayer @Type expr
        toRemove   <- mapM source $ tp : inps
        delete expr
        mapM_ deleteSubtree $ filter (/= expr) toRemove
    else return ()
