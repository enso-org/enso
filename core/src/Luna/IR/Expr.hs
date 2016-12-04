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
import Luna.IR.Expr.Layout.ENT (ENT, ET, NT)


type ET' e = ET e Star
type NT' n = NT n Star

-- class                                         LitExpr m a              where litExpr :: a -> m (Expr (DefListLayout m a))
-- instance (IRMonad m, Accessible ExprNet m) => LitExpr m Prelude.String where litExpr = string ; {-# INLINE litExpr #-}
-- instance Monad m                           => LitExpr m (Expr l)       where litExpr = return ; {-# INLINE litExpr #-}


star :: (IRMonad m, Accessible ExprNet m, Inferable2 Layout ldef m) => m (Expr Star)
star = expr Term.uncheckedStar
{-# INLINE star #-}

rawString :: (IRMonad m, Accessible ExprNet m) => Prelude.String -> m (Expr $ ET' String)
rawString = expr . Term.uncheckedString ; {-# INLINE rawString #-}

-- cons :: (IRMonad m, Accessible ExprNet m) => Expr n -> m (Expr (NT' (Cons :> n)))
cons :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet]) => Expr n -> m (Expr $ NT' $ Cons :> n)
cons n = mdo
    t  <- expr $ Term.uncheckedCons ln
    ln <- link (unsafeGeneralize n) t
    return t

-- string :: (IRMonad m, Accessible ExprNet m) => Prelude.String -> m (Expr (String %> Infered Layout m))
-- string = expr . Term.uncheckedString ; {-# INLINE string #-}
--
-- acc :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet], LitExpr m name)
--     => name -> Expr l -> m (Expr (DefListLayout m name #> l))
-- acc name arg = mdo
--     t  <- expr $ Term.uncheckedAcc ln la
--     n  <- litExpr name
--     ln <- link (unsafeGeneralize n)   t
--     la <- link (unsafeGeneralize arg) t
--     return t
--

to powinnimsy zrobic jakos tak

var :: Expr n -> m (Expr $ Var :> NT' n)

ale tak by bylo to spokjne z Sub'ami!
var :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
    => Expr n -> m (Expr $ NT' $ Var :> n)
var n = mdo
    t <- expr $ Term.uncheckedVar l
    l <- link (unsafeGeneralize n) t
    return t
--
-- unify :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
--       => Expr l -> Expr l' -> m (Expr (Unify |> (l <+> l')))
-- unify a b = mdo
--     t  <- expr $ Term.uncheckedUnify la lb
--     la <- link (unsafeGeneralize a) t
--     lb <- link (unsafeGeneralize b) t
--     return t
-- {-# INLINE unify #-}

-- Var :> ENT . . .
--
--
-- Expr (Ent Star () Star)
--
-- Var :> NT'
--
--
-- e :: Expr $ ENT (Unify :> Draft) (Unify :> Draft) (Unify :> Draft)
--
-- head e :: Unify
-- src  e :: Draft
-- nm   e :: Unify :> Draft
-- tp   e :: Unify :> Draft
--
--
-- e :: Expr $ Unify :> ENT Val Val Val
--
-- head e :: Unify
-- src  e :: Val
-- nm   e :: Val
-- tp   e :: Val
--
--
-- Expr $ ENT (Star :> ENT)
