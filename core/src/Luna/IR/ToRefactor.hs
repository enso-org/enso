module Luna.IR.ToRefactor where

import Luna.Prelude hiding (String)
import qualified Luna.Prelude as Prelude

import Luna.IR.Internal.IR
import qualified Luna.IR.Expr.Term.Named as Term
import qualified Luna.IR.Internal.LayerStore as Store
import Luna.IR.Expr.Layout.Class
import Luna.IR.Expr.Layout.ENT
import Luna.IR.Layer
import Luna.IR.Layer.Type
import Luna.IR.Layer.Model
import Luna.IR.Layer.UID
import Luna.IR.Layer.Succs
import Luna.IR.Expr.Term.Named
import Luna.IR.Expr.Format
import Luna.IR.Expr.Atom
import Data.Property

import Unsafe.Coerce (unsafeCoerce)

-- FIXME: moze po prostu Expr Star?
newtype MagicStar = MagicStar Expr'
makeWrapped ''MagicStar

newMagicStar :: IRMonad m => m MagicStar
newMagicStar = wrap' <$> magicExpr Term.uncheckedStar ; {-# INLINE newMagicStar #-}

magicStar :: Iso' (Expr l) MagicStar
magicStar = iso (wrap' . unsafeCoerce) (unsafeCoerce . unwrap') ; {-# INLINE magicStar #-}









consTypeLayer :: IRMonad m
              => Store.STRefM m (Maybe MagicStar) -> Expr t -> Definition (Expr t) -> m (LayerData Type (Expr t))
consTypeLayer ref self _ = do
    top  <- view (from magicStar) <$> localTop ref
    conn <- magicLink top self
    return conn


localTop :: IRMonad m
         => Store.STRefM m (Maybe MagicStar) -> m MagicStar
localTop ref = Store.readSTRef ref >>= \case
    Just t  -> return t
    Nothing -> mdo
        Store.writeSTRef ref $ Just s
        s <- newMagicStar
        Store.writeSTRef ref Nothing
        return s


-- TODO[WD]: dont allow here to use registerGenericLayer!
--           maybe LayerConsPasses will help here?
layerReg4 :: IRMonad m => m ()
layerReg4 = registerElemLayer @EXPR @Type . consTypeLayer =<< runInIR (Store.newSTRef Nothing)




runRegs :: IRMonad m => m ()
runRegs = do
    runElemRegs
    runLayerRegs

-- === Elem reg defs === --

elemRegs :: IRMonad m => [m ()]
elemRegs = [elemReg1, elemReg2]

runElemRegs :: IRMonad m => m ()
runElemRegs = sequence_ elemRegs

elemReg1 :: IRMonad m => m ()
elemReg1 = registerElem @EXPR

elemReg2 :: IRMonad m => m ()
elemReg2 = registerElem @(LINK' EXPR)


-- === Layer reg defs === --

layerRegs :: IRMonad m => [m ()]
layerRegs = [layerReg1, layerReg2, layerReg3, layerReg4]

runLayerRegs :: IRMonad m => m ()
runLayerRegs = sequence_ layerRegs


layerReg1 :: IRMonad m => m ()
layerReg1 = registerGenericLayer @Model $ \ _ -> return

layerReg2 :: IRMonad m => m ()
layerReg2 = registerGenericLayer @Succs $ \ _ _ -> return def



consUIDLayer :: PrimMonad m => Store.STRefM m ID -> t -> Definition t -> m (LayerData UID t)
consUIDLayer ref _ _ = Store.modifySTRef' ref (\i -> (i, succ i))

layerReg3 :: IRMonad m => m ()
layerReg3 = registerGenericLayer @UID . consUIDLayer =<< runInIR (Store.newSTRef 0)




----------------------------------
----------------------------------
----------------------------------


source :: (IRMonad m, Readable (Layer (Abstract (Link a b)) Model) m) => Link a b -> m a
source = fmap fst . readLayer @Model ; {-# INLINE source #-}



type ExprLink a b = Link (Expr a) (Expr b)
-- strName :: _ => _
strName v = getName v >>= \n -> match' n >>= \ (Term.Sym_String s) -> return s



-- === KnownExpr === --

type KnownExpr l m = (IRMonad m, Readables m '[ExprLayer Model, ExprLinkLayer Model]) -- CheckAtomic (ExprHead l))

match' :: forall l m. KnownExpr l m => Expr l -> m (ExprHeadDef l)
match' = unsafeToExprTermDef @(ExprHead l)

getSource :: KnownExpr l m => Lens' (ExprHeadDef l) (ExprLink a b) -> Expr l -> m (Expr a)
getSource f v = match' v >>= source . view f ; {-# INLINE getSource #-}


-- === KnownName === --

type       KnownName l m = (KnownExpr l m, HasName (ExprHeadDef l))
getName :: KnownName l m => Expr l -> m (Expr (Sub Name l))
getName = getSource name ; {-# INLINE getName #-}







type family Head a

type instance Access EXPR (ENT e _ _) = e
type instance Head (Atomic a) = Atomic a

type ExprHead l = Head (l # EXPR)
type ExprHeadDef l = ExprTermDef (ExprHead l) (Expr l)
