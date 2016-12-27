{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr (module Luna.IR.Expr, module X) where

-- import Luna.IR.Expr.Term as X hiding (match)


import qualified Luna.Prelude as Prelude
import Luna.Prelude hiding (String, Integer, Rational)

import Luna.IR.Expr.Atom   as X
import Luna.IR.Internal.IR as X
import qualified Luna.IR.Expr.Term.Named as Term
-- import Luna.IR.Expr.Layout.ENT (type(#>), type(|>))

import Type.Inference
import Luna.IR.Expr.Layout
import Luna.IR.Expr.Layout.ENT hiding (Cons)
import Luna.IR.Function (Arg)
import Data.Event (Emitter, type (//))

type ET' e = ET e Star
type NT' n = NT n Star
type T' = T Star

-- class                                         LitExpr m a              where litExpr :: a -> m (Expr (DefListLayout m a))
-- instance (IRMonad m, Editor NET EXPR m) => LitExpr m Prelude.String where litExpr = string ; {-# INLINE litExpr #-}
-- instance Monad m                           => LitExpr m (Expr l)       where litExpr = return ; {-# INLINE litExpr #-}

-- type NewExpr  m = (MonadPass m, Editor NET EXPR m, Emitter m (NEW // EXPR))
-- type NewExpr' m = (MonadPass m, Editors NET '[EXPR, LINK' EXPR] m, Emitter m (NEW // EXPR), Emitter m (NEW // LINK' EXPR))

-- star :: NewExpr m => m (Expr Star)
-- star = expr Term.uncheckedStar
-- {-# INLINE star #-}

star2 :: (MonadPass m, Editor NET EXPR m, Emitter m (NEW // EXPR)) => m (Expr Star)
star2 = expr2 Term.uncheckedStar
{-# INLINE star2 #-}

reserveStar :: (MonadPass m, Editor NET EXPR m) => m (Expr Star)
reserveStar = reserveExpr ; {-# INLINE reserveStar #-}

registerStar :: Emitter m (NEW // EXPR) => Expr Star -> m ()
registerStar = dispatchNewExpr2 Term.uncheckedStar

--
-- string :: NewExpr m => Prelude.String -> m (Expr String)
-- string = expr . Term.uncheckedString ; {-# INLINE string #-}
--
-- integer :: (NewExpr m, Integral a) => a -> m (Expr Integer)
-- integer = expr . Term.uncheckedInteger . fromIntegral ; {-# INLINE integer #-}
--
-- rational :: NewExpr m => Prelude.Rational -> m (Expr Rational)
-- rational = expr . Term.uncheckedRational ; {-# INLINE rational #-}
--
-- -- cons :: NewExpr m => Expr n -> m (Expr (NT' (Cons >> n)))
-- cons :: NewExpr' m => Expr n -> m (Expr $ Cons #> n)
-- cons n = mdo
--     t  <- expr $ Term.uncheckedCons ln
--     ln <- link (unsafeRelayout n) t
--     return t
--
-- blank :: NewExpr m => m (Expr Blank)
-- blank = expr Term.uncheckedBlank
--
-- -- string :: (IRMonad m, Editor NET EXPR m) => Prelude.String -> m (Expr (String %> Infered Layout m))
-- -- string = expr . Term.uncheckedString ; {-# INLINE string #-}
-- --
-- -- acc :: (IRMonad m, Editors m '[NET EXPR, ExprLinkNet], LitExpr m name)
-- --     => name -> Expr l -> m (Expr (DefListLayout m name #> l))
-- -- acc name arg = mdo
-- --     t  <- expr $ Term.uncheckedAcc ln la
-- --     n  <- litExpr name
-- --     ln <- link (unsafeRelayout n)   t
-- --     la <- link (unsafeRelayout arg) t
-- --     return t
-- --
--
-- -- to powinnimsy zrobic jakos tak
--
-- -- var :: Expr n -> m (Expr $ Var >> NT' n)
--
-- -- ale tak by bylo to spokjne z Sub'ami!
-- var :: NewExpr' m => Expr n -> m (Expr $ Var #> n)
-- var n = mdo
--     t <- expr $ Term.uncheckedVar l
--     l <- link (unsafeRelayout n) t
--     return t
--
-- -- TODO[MK, WD]: I guess at some point it won't be necessary, but for now let's have this helper fun
-- strVar :: NewExpr' m => Prelude.String -> m (Expr $ Var #> String)
-- strVar name = string name >>= var
--
-- acc :: NewExpr' m => Expr n -> Expr t -> m (Expr $ Acc >> t #> n)
-- acc n b = mdo
--     t  <- expr $ Term.uncheckedAcc ln lb
--     ln <- link (unsafeRelayout n) t
--     lb <- link (unsafeRelayout b) t
--     return t
--
-- rawAcc :: NewExpr' m => Prelude.String -> Expr t -> m (Expr $ Acc >> t #> String)
-- rawAcc name b = string name >>= flip acc b
--
-- unify :: NewExpr' m => Expr l -> Expr l' -> m (Expr $ Unify >> (l <+> l'))
-- unify a b = mdo
--     t  <- expr $ Term.uncheckedUnify la lb
--     la <- link (unsafeRelayout a) t
--     lb <- link (unsafeRelayout b) t
--     return t
-- {-# INLINE unify #-}
--
-- app :: NewExpr' m => Expr l -> Arg (Expr l') -> m (Expr $ App >> (l <+> l'))
-- app f a = mdo
--     t  <- expr $ Term.uncheckedApp lf la
--     lf <- link (unsafeRelayout f) t
--     la <- mapM (flip link t . unsafeRelayout) a
--     return t
--
-- lam :: NewExpr' m => Arg (Expr l) -> Expr l' -> m (Expr $ Lam >> (l <+> l'))
-- lam i o = mdo
--     t  <- expr $ Term.uncheckedLam li lo
--     li <- mapM (flip link t . unsafeRelayout) i
--     lo <- link (unsafeRelayout o) t
--     return t
--
-- grouped :: NewExpr' m => Expr l -> m (Expr $ Grouped >> E l)
-- grouped e = mdo
--     t  <- expr $ Term.uncheckedGrouped le
--     le <- link e t
--     return t
--
--
-- -- Var >> ENT . . .
-- --
-- --
-- -- Expr (Ent Star () Star)
-- --
-- -- Var >> NT'
-- --
-- --
-- -- e :: Expr $ ENT (Unify >> Draft) (Unify >> Draft) (Unify >> Draft)
-- --
-- -- head e :: Unify
-- -- src  e :: Draft
-- -- nm   e :: Unify >> Draft
-- -- tp   e :: Unify >> Draft
-- --
-- --
-- -- e :: Expr $ Unify >> ENT Val Val Val
-- --
-- -- head e :: Unify
-- -- src  e :: Val
-- -- nm   e :: Val
-- -- tp   e :: Val
-- --
-- --
-- -- Expr $ ENT (Star >> ENT)
