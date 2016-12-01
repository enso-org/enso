{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr (module Luna.IR.Expr, module X) where

-- import Luna.IR.Expr.Term as X hiding (match)


import qualified Luna.Prelude as Prelude
import Luna.Prelude hiding (String)

import Luna.IR.Expr.Atom   as X
import Luna.IR.Internal.IR as X
import qualified Luna.IR.Expr.Term.Named as Term
import Luna.IR.Expr.Layout.ENT (type(#>), type(|>))

import Type.Inference
import Luna.IR.Expr.Layout



class                                         LitExpr m a              where litExpr :: a -> m (Expr (DefListLayout m a))
instance (IRMonad m, Accessible ExprNet m) => LitExpr m Prelude.String where litExpr = string ; {-# INLINE litExpr #-}
instance Monad m                           => LitExpr m (Expr l)       where litExpr = return ; {-# INLINE litExpr #-}


star :: (IRMonad m, Accessible ExprNet m, Inferable2 Layout ldef m) => m (Expr (AtomLayout Star ldef))
star = expr Term.uncheckedStar
{-# INLINE star #-}

string :: (IRMonad m, Accessible ExprNet m) => Prelude.String -> m (Expr (String %> Infered Layout m))
string = expr . Term.uncheckedString ; {-# INLINE string #-}

acc :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet], LitExpr m name)
    => name -> Expr l -> m (Expr (DefListLayout m name #> l))
acc name arg = mdo
    t  <- expr $ Term.uncheckedAcc ln la
    n  <- litExpr name
    ln <- link (unsafeGeneralize n)   t
    la <- link (unsafeGeneralize arg) t
    return t

var :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet], Inferable2 Layout ldef m, LitExpr m name)
    => name -> m (Expr (DefListLayout m name #> AtomLayout Var ldef))
var name = mdo
    t <- expr $ Term.uncheckedVar l
    n <- litExpr name
    l <- link (unsafeGeneralize n) t
    return t

unify :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
      => Expr l -> Expr l' -> m (Expr (Unify |> (l <+> l')))
unify a b = mdo
    t  <- expr $ Term.uncheckedUnify la lb
    la <- link (unsafeGeneralize a) t
    lb <- link (unsafeGeneralize b) t
    return t
{-# INLINE unify #-}
