{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}

module Luna.IR.ToRefactor where

import Luna.Prelude hiding (String, log, nested)
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
import Luna.IR.Expr.Term.Named (HasName, name)
import Luna.IR.Expr.Format
import Luna.IR.Expr.Atom
import Data.Property
import qualified Luna.Pass        as Pass
import           Luna.Pass        (Pass, Inputs, Outputs, Events, Preserves, SubPass)
import Data.TypeVal
import Data.Event (Emitter, type (//))
import qualified Data.Set as Set
import Luna.IR.Internal.LayerStore (STRefM)
import Luna.IR.Expr
import Unsafe.Coerce (unsafeCoerce)
import Luna.Pass.Manager as PM
import Data.Event as Event
import System.Log
import qualified Control.Monad.State.Dependent.Old as DepState

---------------------------------------
-- Some important utils



type family PassAttr attr pass

type instance KeyData (Pass.SubPass pass m) (Attr a) = PassAttr a pass





data Abstracted a
type instance Abstract (TypeRef s) = TypeRef (Abstracted s)




data ELEMSCOPE p elem
data ElemScope p elem
type instance Abstract (ElemScope c t) = ELEMSCOPE (Abstract c) (Abstract t)
type instance PassAttr WorkingElem (ElemScope p t) = (Elem t, Definition (Elem t))

type ElemSubPass p elem   = SubPass (ElemScope p elem)
type ElemPass    p elem m = ElemSubPass p elem m ()

proxifyElemPass :: ElemSubPass p elem m a -> (Proxy elem -> ElemSubPass p elem m a)
proxifyElemPass = const ; {-# INLINE proxifyElemPass #-}




instance (Monad m, Event.FromPath e, m ~ GetBaseMonad n) => KeyMonad (Event e) (PassManager m) n where
    uncheckedLookupKey = Just . Key . fixme1 . sequence . fmap Pass.eval <$> PM.queryListeners (Event.fromPath @e)
    -- FIXME[WD]: Pass.eval and sequence_ just hide error if some keys were not found

fixme1 :: Monad m => m [Either Pass.InternalError ()] -> m ()
fixme1 m = fromRight =<< (sequence <$> m)
fromRight (Right a) = return ()
fromRight (Left e) = error $ show e


-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------
-- Layer passes


debugElem :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> m ()
debugElem t msg = debug $ show (typeVal' @(Abstract t) :: TypeRep) <> " [" <> show (t ^. idx) <> "]: " <> msg

debugLayerCreation :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> Prelude.String -> m ()
debugLayerCreation t layer post = debugElem t $ layer <> " layer created" <> post

debugLayerCreation' :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> m ()
debugLayerCreation' t layer = debugLayerCreation t layer ""


-------------------
-- === Model === --
-------------------

data InitModel
type instance Abstract InitModel = InitModel
type instance Inputs    (ElemScope InitModel t) = '[Layer (Abstract t) Model, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs   (ElemScope InitModel t) = '[Layer (Abstract t) Model]
type instance Events    (ElemScope InitModel t) = '[]
type instance Preserves (ElemScope InitModel t) = '[]

initModel :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Pass (ElemScope InitModel t) m
initModel = do
    (t, tdef) <- readAttr @WorkingElem
    flip (writeLayer @Model) t tdef
    debugLayerCreation' t "Model"

initModel_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => TypeRep -> Pass.DynPass m
initModel_dyn = reifyKnownTypeT @Abstracted $ Pass.compile <$> proxifyElemPass initModel

initModel_reg :: (IRMonad m, MonadIO m) => PassManager m ()
initModel_reg = registerLayer (typeVal' @Model) initModel_dyn

instance MonadLogging m => MonadLogging (SubPass pass m)
instance MonadLogging m => MonadLogging (PassManager  m)


-----------------
-- === UID === --
-----------------

data InitUID
type instance Abstract InitUID = InitUID
type instance Inputs    (ElemScope InitUID t) = '[Layer (Abstract t) UID, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs   (ElemScope InitUID t) = '[Layer (Abstract t) UID]
type instance Events    (ElemScope InitUID t) = '[]
type instance Preserves (ElemScope InitUID t) = '[]

initUID :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => STRefM m ID -> Pass (ElemScope InitUID t) m
initUID ref = do
    (t, tdef) <- readAttr @WorkingElem
    nuid <- Store.modifySTRef' ref (\i -> (i, succ i))
    flip (writeLayer @UID) t nuid
    debugLayerCreation t "UID" $ " (" <> show nuid <> ")"


initUID_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => m (TypeRep -> Pass.DynPass m)
initUID_dyn = do
    r <- Store.newSTRef 0
    return $ reifyKnownTypeT @Abstracted $ Pass.compile <$> proxifyElemPass (initUID r)

initUID_reg :: (IRMonad m, MonadIO m) => PassManager m ()
initUID_reg = registerLayer (typeVal' @UID) =<< initUID_dyn



-------------------
-- === Succs === --
-------------------

data InitSuccs
type instance Abstract InitSuccs = InitSuccs
type instance Inputs    (ElemScope InitSuccs t) = '[Layer (Abstract t) Succs, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs   (ElemScope InitSuccs t) = '[Layer (Abstract t) Succs]
type instance Events    (ElemScope InitSuccs t) = '[]
type instance Preserves (ElemScope InitSuccs t) = '[]

initSuccs :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Pass (ElemScope InitSuccs t) m
initSuccs = do
    (t, _) <- readAttr @WorkingElem
    flip (writeLayer @Succs) t mempty
    debugLayerCreation' t "Succs"

initSuccs_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => TypeRep -> Pass.DynPass m
initSuccs_dyn = reifyKnownTypeT @Abstracted $ Pass.compile <$> proxifyElemPass initSuccs

initSuccs_reg :: (IRMonad m, MonadIO m) => PassManager m ()
initSuccs_reg = registerLayer (typeVal' @Succs) initSuccs_dyn


data WatchSuccs
type instance Abstract WatchSuccs = WatchSuccs
type instance Inputs    (ElemScope WatchSuccs t) = '[ExprLayer Succs, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs   (ElemScope WatchSuccs t) = '[ExprLayer Succs]
type instance Events    (ElemScope WatchSuccs t) = '[]
type instance Preserves (ElemScope WatchSuccs t) = '[]

watchSuccs :: forall l m. (MonadIO m, IRMonad m) => Pass (ElemScope WatchSuccs (Link' (Expr l))) m
watchSuccs = do
    (t, (src, tgt)) <- readAttr @WorkingElem
    debugElem t $ "Updating successor: " <> show (src ^. idx) <>" -> " <> show (tgt ^. idx)
    modifyLayer_ @Succs (Set.insert $ generalize tgt) src

watchSuccs_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => Pass.DynPass m
watchSuccs_dyn = Pass.compile $ watchSuccs



------------------
-- === Type === --
------------------

consTypeLayer :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet], Emitter m (NEW // LINK' EXPR), Emitter m (NEW // EXPR))
              => Store.STRefM m (Maybe (Expr Star)) -> Expr t -> m (LayerData Type (Expr t))
consTypeLayer ref self = do
    top  <- unsafeRelayout <$> localTop ref
    link top self


localTop :: (IRMonad m, Accessible ExprNet m, Emitter m (NEW // EXPR))
         => Store.STRefM m (Maybe (Expr Star)) -> m (Expr Star)
localTop ref = Store.readSTRef ref >>= \case
    Just t  -> return t
    Nothing -> do
        s <- reserveStar
        Store.writeSTRef ref $ Just s
        registerStar s
        Store.writeSTRef ref Nothing
        return s


data InitType
type instance Abstract InitType = InitType
type instance Inputs    (ElemScope InitType t) = '[Layer (Abstract t) Type, ExprNet, ExprLinkNet, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs   (ElemScope InitType t) = '[Layer (Abstract t) Type, ExprNet, ExprLinkNet]
type instance Events    (ElemScope InitType t) = '[NEW // EXPR, NEW // LINK' EXPR]
type instance Preserves (ElemScope InitType t) = '[]

initType :: forall l m. (MonadIO m, IRMonad m, MonadPassManager m) => Store.STRefM m (Maybe (Expr Star)) -> Pass (ElemScope InitType (EXPRESSION l)) m
initType ref = do
    (el, _) <- readAttr @WorkingElem
    debugElem el "Type construction"
    nested $ do
        t <- consTypeLayer ref el
        flip (writeLayer @Type) el t
    debugLayerCreation' el "Type"

-- | Notice! This pass mimics signature needed by proto and the input TypeRep is not used
--   because it only works for Expressions
-- initType_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => TypeRep -> Pass.DynPass m
initType_dyn = do
    r <- Store.newSTRef Nothing
    return $ \ _ -> Pass.compile $ initType r

-- initType_reg :: (IRMonad m, MonadIO m) => PassManager m ()
initType_reg = registerLayer (typeVal' @Type) =<< initType_dyn








-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------

attachLayer priority l e = attachLayerIR l e >> attachLayerPM priority l e


-- FIXME [WD]: is the type ugly here?
-- runRegs :: _ => _
runRegs = do
    runElemRegs

    initModel_reg
    attachLayer 0 (typeVal' @Model) (typeVal' @EXPR)
    attachLayer 0 (typeVal' @Model) (typeVal' @(LINK' EXPR))
    --
    initUID_reg
    attachLayer 5 (typeVal' @UID) (typeVal' @EXPR)
    attachLayer 5 (typeVal' @UID) (typeVal' @(LINK' EXPR))

    initSuccs_reg
    attachLayer 5 (typeVal' @Succs) (typeVal' @EXPR)
    --
    initType_reg
    attachLayer 10 (typeVal' @Type) (typeVal' @EXPR)

    addEventListener 100 (NEW // LINK EXPR EXPR) watchSuccs


-- === Elem reg defs === --

runElemRegs :: IRMonad m => m ()
runElemRegs = sequence_ [elemReg1, elemReg2, elemReg3]

elemReg1 :: IRMonad m => m ()
elemReg1 = registerElem @EXPR

elemReg2 :: IRMonad m => m ()
elemReg2 = registerElem @(LINK' EXPR)

elemReg3 :: IRMonad m => m ()
elemReg3 = registerElem @(GROUP EXPR)


-- === Layer reg defs === --

layerRegs :: IRMonad m => [m ()]
layerRegs = [] -- [layerReg1, layerReg2, layerReg3, layerReg4]

runLayerRegs :: IRMonad m => m ()
runLayerRegs = sequence_ layerRegs





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

modifyExprTerm :: forall l m. (KnownExpr l m, Writable (ExprLayer Model) m) => Expr l -> (ExprHeadDef l -> ExprHeadDef l) -> m ()
modifyExprTerm = unsafeModifyExprTermDef @(ExprHead l)

getSource :: KnownExpr l m => Lens' (ExprHeadDef l) (ExprLink a b) -> Expr l -> m (Expr a)
getSource f v = match' v >>= source . view f ; {-# INLINE getSource #-}


-- === KnownName === --

type       KnownName l m = (KnownExpr l m, HasName (ExprHeadDef l))
getName :: KnownName l m => Expr l -> m (Expr (Sub NAME l))
getName = getSource name ; {-# INLINE getName #-}







type family Head a

type instance Access EXPR (ENT e _ _) = e
type instance Access EXPR (E   e    ) = e
type instance Head (Atomic a) = Atomic a

type ExprHead l = Head (l # EXPR)
type ExprHeadDef l = ExprTermDef (ExprHead l) (Expr l)



---------- TRASH
------ TO BE DELETED WHEN POSSIBLE

instance MonadLogging m => MonadLogging (DepState.StateT a b m)
