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


type ET' e = ET e Star
type NT' n = NT n Star
type T' = T Star

-- class                                         LitExpr m a              where litExpr :: a -> m (Expr (DefListLayout m a))
-- instance (IRMonad m, Accessible ExprNet m) => LitExpr m Prelude.String where litExpr = string ; {-# INLINE litExpr #-}
-- instance Monad m                           => LitExpr m (Expr l)       where litExpr = return ; {-# INLINE litExpr #-}


star :: (IRMonad m, Accessible ExprNet m) => m (Expr Star)
star = expr Term.uncheckedStar
{-# INLINE star #-}

rawString :: (IRMonad m, Accessible ExprNet m) => Prelude.String -> m (Expr String)
rawString = expr . Term.uncheckedString ; {-# INLINE rawString #-}

integer :: (IRMonad m, Accessible ExprNet m, Integral a) => a -> m (Expr Integer)
integer = expr . Term.uncheckedInteger . fromIntegral ; {-# INLINE integer #-}

rational :: (IRMonad m, Accessible ExprNet m) => Prelude.Rational -> m (Expr Rational)
rational = expr . Term.uncheckedRational ; {-# INLINE rational #-}

-- cons :: (IRMonad m, Accessible ExprNet m) => Expr n -> m (Expr (NT' (Cons >> n)))
cons :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet]) => Expr n -> m (Expr $ Cons #> n)
cons n = mdo
    t  <- expr $ Term.uncheckedCons ln
    ln <- link (unsafeRelayout n) t
    return t

blank :: (IRMonad m, Accessible ExprNet m) => m (Expr Blank)
blank = expr Term.uncheckedBlank

-- string :: (IRMonad m, Accessible ExprNet m) => Prelude.String -> m (Expr (String %> Infered Layout m))
-- string = expr . Term.uncheckedString ; {-# INLINE string #-}
--
-- acc :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet], LitExpr m name)
--     => name -> Expr l -> m (Expr (DefListLayout m name #> l))
-- acc name arg = mdo
--     t  <- expr $ Term.uncheckedAcc ln la
--     n  <- litExpr name
--     ln <- link (unsafeRelayout n)   t
--     la <- link (unsafeRelayout arg) t
--     return t
--

-- to powinnimsy zrobic jakos tak

-- var :: Expr n -> m (Expr $ Var >> NT' n)

-- ale tak by bylo to spokjne z Sub'ami!
var :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
    => Expr n -> m (Expr $ Var #> n)
var n = mdo
    t <- expr $ Term.uncheckedVar l
    l <- link (unsafeRelayout n) t
    return t

-- TODO[MK, WD]: I guess at some point it won't be necessary, but for now let's have this helper fun
rawVar :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
       => Prelude.String -> m (Expr $ Var #> String)
rawVar name = rawString name >>= var

acc :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
    => Expr n -> Expr t -> m (Expr $ Acc >> t #> n)
acc n b = mdo
    t  <- expr $ Term.uncheckedAcc ln lb
    ln <- link (unsafeRelayout n) t
    lb <- link (unsafeRelayout b) t
    return t

rawAcc :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
       => Prelude.String -> Expr t -> m (Expr $ Acc >> t #> String)
rawAcc name b = rawString name >>= flip acc b

unify :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
      => Expr l -> Expr l' -> m (Expr $ Unify >> (l <+> l'))
unify a b = mdo
    t  <- expr $ Term.uncheckedUnify la lb
    la <- link (unsafeRelayout a) t
    lb <- link (unsafeRelayout b) t
    return t
{-# INLINE unify #-}

app :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
    => Expr l -> Arg (Expr l') -> m (Expr $ App >> (l <+> l'))
app f a = mdo
    t  <- expr $ Term.uncheckedApp lf la
    lf <- link (unsafeRelayout f) t
    la <- mapM (flip link t . unsafeRelayout) a
    return t

lam :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
    => Arg (Expr l) -> Expr l' -> m (Expr $ Lam >> (l <+> l'))
lam i o = mdo
    t  <- expr $ Term.uncheckedLam li lo
    li <- mapM (flip link t . unsafeRelayout) i
    lo <- link (unsafeRelayout o) t
    return t

grouped :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet])
    => Expr l -> m (Expr $ Grouped >> E l)
grouped e = mdo
    t  <- expr $ Term.uncheckedGrouped le
    le <- link e t
    return t


-- Var >> ENT . . .
--
--
-- Expr (Ent Star () Star)
--
-- Var >> NT'
--
--
-- e :: Expr $ ENT (Unify >> Draft) (Unify >> Draft) (Unify >> Draft)
--
-- head e :: Unify
-- src  e :: Draft
-- nm   e :: Unify >> Draft
-- tp   e :: Unify >> Draft
--
--
-- e :: Expr $ Unify >> ENT Val Val Val
--
-- head e :: Unify
-- src  e :: Val
-- nm   e :: Val
-- tp   e :: Val
--
--
-- Expr $ ENT (Star >> ENT)
