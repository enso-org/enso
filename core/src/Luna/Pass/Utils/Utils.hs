module Luna.Pass.Utils.Utils where

import           Luna.Prelude
import qualified Luna.Pass    as Pass
import           Luna.IR
import           Data.TypeVal

narrowAtom :: forall a m. (IRMonad m, KnownType (AtomOf a), Readable (ExprLayer Model) m)
           => AnyExpr -> m (Maybe (Expr (AtomOf a)))
narrowAtom expr = do
    exprAtomRep <- getAtomRep expr
    return $ if exprAtomRep == atomRep' @a
      then Just $ unsafeGeneralize expr
      else Nothing
