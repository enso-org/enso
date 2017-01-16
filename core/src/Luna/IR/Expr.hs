{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Expr (module Luna.IR.Expr, module X) where

-- import Luna.IR.Expr.Term as X hiding (match)


import qualified Luna.Prelude as Prelude
import Luna.Prelude hiding (String, Integer, Rational, cons)

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
-- instance (IRMonad m, Editor Net AnyExpr m) => LitExpr m Prelude.String where litExpr = string ; {-# INLINE litExpr #-}
-- instance Monad m                           => LitExpr m (Expr l)       where litExpr = return ; {-# INLINE litExpr #-}

-- type NewExpr  m = (MonadRef m, Editor Net AnyExpr m, Emitter m (New // AnyExpr))
-- type NewExpr' m = (MonadRef m, Editors Net '[AnyExpr, LINK' AnyExpr] m, Emitter m (New // AnyExpr), Emitter m (New // LINK' AnyExpr))

-- star :: NewExpr m => m (Expr Star)
-- star = expr Term.uncheckedStar
-- {-# INLINE star #-}

star :: (MonadRef m, Writer Net AnyExpr m, NewElemEvent m (Expr Star)) => m (Expr Star)
star = expr Term.uncheckedStar
{-# INLINE star #-}

reserveStar :: (MonadRef m, Writer Net AnyExpr m) => m (Expr Star)
reserveStar = reserveExpr ; {-# INLINE reserveStar #-}

registerStar :: NewElemEvent m (Expr Star) => Expr Star -> m ()
registerStar = dispatchNewExpr Term.uncheckedStar ; {-# INLINE registerStar #-}


string :: (MonadRef m, Writer Net AnyExpr m, NewElemEvent m (Expr String)) => Prelude.String -> m (Expr String)
string = expr . Term.uncheckedString ; {-# INLINE string #-}

integer :: (MonadRef m, Writer Net AnyExpr m, NewElemEvent m (Expr Integer), Integral a) => a -> m (Expr Integer)
integer = expr . Term.uncheckedInteger . fromIntegral ; {-# INLINE integer #-}

rational :: (MonadRef m, Writer Net AnyExpr m, NewElemEvent m (Expr Rational)) => Prelude.Rational -> m (Expr Rational)
rational = expr . Term.uncheckedRational ; {-# INLINE rational #-}

-- cons :: NewExpr m => Expr n -> m (Expr (NT' (Cons >> n)))
cons :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Cons), NewElemEvent m SomeExprLink) => Expr n -> [Arg (Expr t)] -> m (Expr $ Cons >> t #> n)
cons n fs = mdo
    t  <- expr $ Term.uncheckedCons ln fn
    ln <- link (unsafeRelayout n) t
    fn <- (mapM . mapM) (flip link t . unsafeRelayout) fs
    return t

cons_ :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Cons), NewElemEvent m SomeExprLink) => Expr n -> m (Expr $ Cons >> t #> n)
cons_ = flip cons []

blank :: (MonadRef m, Writer Net AnyExpr m, NewElemEvent m (Expr Blank)) => m (Expr Blank)
blank = expr Term.uncheckedBlank

-- string :: (IRMonad m, Editor Net AnyExpr m) => Prelude.String -> m (Expr (String %> Infered Layout m))
-- string = expr . Term.uncheckedString ; {-# INLINE string #-}
--
-- acc :: (IRMonad m, Editors m '[Net AnyExpr, ExprLinkNet], LitExpr m name)
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
var :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Var), NewElemEvent m SomeExprLink) => Expr n -> m (Expr $ Var #> n)
var n = mdo
    t <- expr $ Term.uncheckedVar l
    l <- link (unsafeRelayout n) t
    return t

-- TODO[MK, WD]: I guess at some point it won't be necessary, but for now let's have this helper fun
strVar :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Var), NewElemEvent m SomeExprLink) => Prelude.String -> m (Expr $ Var #> String)
strVar name = string name >>= var

acc :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Acc), NewElemEvent m SomeExprLink) => Expr n -> Expr t -> m (Expr $ Acc >> t #> n)
acc n b = mdo
    t  <- expr $ Term.uncheckedAcc ln lb
    ln <- link (unsafeRelayout n) t
    lb <- link (unsafeRelayout b) t
    return t

rawAcc :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Acc), NewElemEvent m SomeExprLink) => Prelude.String -> Expr t -> m (Expr $ Acc >> t #> String)
rawAcc name b = string name >>= flip acc b

unify :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Unify), NewElemEvent m SomeExprLink) => Expr l -> Expr l' -> m (Expr $ Unify >> (l <+> l'))
unify a b = mdo
    t  <- expr $ Term.uncheckedUnify la lb
    la <- link (unsafeRelayout a) t
    lb <- link (unsafeRelayout b) t
    return t
{-# INLINE unify #-}

app :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr App), NewElemEvent m SomeExprLink) => Expr l -> Arg (Expr l') -> m (Expr $ App >> (l <+> l'))
app f a = mdo
    t  <- expr $ Term.uncheckedApp lf la
    lf <- link (unsafeRelayout f) t
    la <- mapM (flip link t . unsafeRelayout) a
    return t

lam :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Lam), NewElemEvent m SomeExprLink) => Arg (Expr l) -> Expr l' -> m (Expr $ Lam >> (l <+> l'))
lam i o = mdo
    t  <- expr $ Term.uncheckedLam li lo
    li <- mapM (flip link t . unsafeRelayout) i
    lo <- link (unsafeRelayout o) t
    return t

grouped :: (MonadRef m, Writer Net AnyExpr m, Writer Net AnyExprLink m, NewElemEvent m (Expr Grouped), NewElemEvent m SomeExprLink) => Expr l -> m (Expr $ Grouped >> E l)
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
