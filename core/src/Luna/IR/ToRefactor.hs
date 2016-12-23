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
import           Luna.Pass        (Pass, Preserves, Inputs, Outputs, Events, SubPass, Initializer, Template, DynPass3, ElemScope2, KnownElemPass, elemPassDescription, genericDescription, genericDescription')
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

import qualified GHC.Prim as Prim

import Data.Reflection (Reifies)

---------------------------------------
-- Some important utils




type instance KeyData ATTR a _ = a





data Abstracted a
type instance Abstract (TypeRef s) = TypeRef (Abstracted s)




-- data ELEMSCOPE p elem
-- data ElemScope p elem
-- type instance Abstract (ElemScope c t) = ELEMSCOPE (Abstract c) (Abstract t)
--
-- type ElemSubPass p elem   = SubPass (ElemScope p elem)
-- type ElemPass    p elem m = ElemSubPass p elem m ()
--
-- proxifyElemPass :: ElemSubPass p elem m a -> (Proxy elem -> ElemSubPass p elem m a)
-- proxifyElemPass = const ; {-# INLINE proxifyElemPass #-}


-- m (m [Template (DynPass3 (GetPassManager m))])

instance IRMonad m => KeyMonad EVENT (PassManager m) where -- Event.FromPath e
    uncheckedLookupKey a = Just . Key <$> (fmap (fmap sequence_ . sequence) . fixme1 . sequence . fmap Pass.runInitializer =<< PM.queryListeners2 (Event.fromPathDyn a))
    -- FIXME[WD]: Pass.eval and sequence_ just hide error if some keys were not found

fixme1 :: Monad m => m [Either Pass.InternalError a] -> m [a]
fixme1 m = fromRight =<< (sequence <$> m)
fromRight (Right a) = return a
fromRight (Left e) = error $ show e


-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------
-- Layer passes




debugElem :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> m ()
debugElem t = debugBy (show (typeVal' @(Abstract t) :: TypeRep) <> " [" <> show (t ^. idx) <> "]")

debugLayerCreation :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> Prelude.String -> m ()
debugLayerCreation t layer post = debugElem t $ layer <> " layer created" <> post

debugLayerCreation' :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> m ()
debugLayerCreation' t layer = debugLayerCreation t layer ""


proxify :: a -> Proxy a
proxify _ = Proxy


-------------------
-- === Model2 === --
-------------------

-- proxifyElemPass2 :: Pass.Template (ElemSubPass p elem m a) -> (Proxy elem -> Pass.Template (ElemSubPass p elem m a))
-- proxifyElemPass2 = const ; {-# INLINE proxifyElemPass2 #-}

-- proxifyElemPass3 :: Pass.Template (SubPass (ElemScope2 p elem) m a) -> (Proxy elem -> Pass.Template (SubPass (ElemScope2 p elem) m a))
-- proxifyElemPass3 = const ; {-# INLINE proxifyElemPass3 #-}

-- type instance Abstract InitModel2 = InitModel2
-- type instance Inputs    (ElemScope InitModel2 t) = '[Layer (Abstract t) Model] -- FIXME[bug: unnecessary inputs needed]
-- type instance Outputs   (ElemScope InitModel2 t) = '[Layer (Abstract t) Model]
-- type instance Events    (ElemScope InitModel2 t) = '[]
-- type instance Preserves (ElemScope InitModel2 t) = '[]
--
-- type instance Abstract InitModel2 = InitModel2
-- type instance Inputs    (ElemScope2 InitModel2 t) = '[Layer (Abstract t) Model] -- FIXME[bug: unnecessary inputs needed]
-- type instance Outputs   (ElemScope2 InitModel2 t) = '[Layer (Abstract t) Model]
-- type instance Events    (ElemScope2 InitModel2 t) = '[]
-- type instance Preserves (ElemScope2 InitModel2 t) = '[]


-- data InitModel2
-- type instance Abstract InitModel2 = InitModel2
-- type instance Desc NET      (ElemScope2 InitModel2 t) = '[RW EXPR, RW $ LINK' EXPR]
-- type instance Desc LAYER    (ElemScope2 InitModel2 t) = '[RW $ Abstract t // Model] -- FIXME[bug: unnecessary inputs needed]
-- type instance Desc ATTR     (ElemScope2 InitModel2 t) = '[]
-- type instance Desc EVENT    (ElemScope2 InitModel2 t) = '[]
-- type instance Desc PRESERVE (ElemScope2 InitModel2 t) = '[]


data InitModel2
type instance Abstract InitModel2 = InitModel2
type instance Inputs  NET      (ElemScope2 InitModel2 t) = '[EXPR, LINK' EXPR]
type instance Outputs NET      (ElemScope2 InitModel2 t) = '[EXPR, LINK' EXPR]
type instance Inputs  LAYER    (ElemScope2 InitModel2 t) = '[Layer (Abstract t) Model] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs LAYER    (ElemScope2 InitModel2 t) = '[Layer (Abstract t) Model] -- FIXME[bug: unnecessary inputs needed]
type instance Inputs  ATTR     (ElemScope2 InitModel2 t) = '[]
type instance Outputs ATTR     (ElemScope2 InitModel2 t) = '[]
type instance Inputs  EVENT    (ElemScope2 InitModel2 t) = '[]
type instance Outputs EVENT    (ElemScope2 InitModel2 t) = '[]
type instance Preserves        (ElemScope2 InitModel2 t) = '[]


instance KnownElemPass InitModel2 where
    elemPassDescription = genericDescription' . proxify
    -- -- initModel2 :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Any -> Pass (ElemScope InitModel2 t) m
    -- -- initModel2 :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Any -> Pass (ElemScope InitModel2 t) m
    -- -- initModel2 :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Listener (NEW // Abstract t) (ElemScope InitModel2 t) m
    -- -- initModel2 :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => EventPass (NEW // t) (ElemScope InitModel2 t) m
    -- -- initModel2 :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => TypeRep -> Pass.Desc (Prim.Any -> DynPass m)
-- initModel2 :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => TypeRep -> Pass.Desc (DynEventPass m)



initModel2 :: forall t m. (IRMonad m, KnownType (Abstract t)) => (Elem t, Definition t) -> Pass (ElemScope2 InitModel2 t) m
initModel2 (t, tdef) = do
    flip (writeLayer @Model) t tdef
    debugLayerCreation' t "Model"

initModel2p :: (MonadPassManager m, KeyMonad EVENT m) => Pass.Proto (Pass.Describbed (Initializer m (Template (Pass.DynPass3 m))))
initModel2p = passT7 $ Pass.template initModel2
    --
    -- initModel3 :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Pass.PassTemplate (ElemScope InitModel2 t) m
    -- initModel3 = Pass.template initModel2
    --
    -- initModel4 :: (MonadIO m, IRMonad m, KnownType (Abstract t)) => Proxy t -> Pass.PassTemplate (ElemScope InitModel2 t) m
    -- initModel4 = proxifyElemPass2 initModel3
    --
    -- initModel5 :: (MonadIO m, IRMonad m, KnownType (Abstract t)) => Proxy t -> Initializer m (Template (DynPass3 m))
    -- initModel5 = fmap Pass.initialize initModel4
    --
    -- initModel6 :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Proxy t -> Pass.Describbed (Initializer m (Template (DynPass3 m)))
    -- initModel6 = fmap (Pass.describbed @(ElemScope InitModel2 t)) initModel5
    --
    -- initModel7 :: (MonadIO m, IRMonad m) => Pass.Proto (Pass.Describbed (Initializer m (Template (DynPass3 m))))
    -- initModel7 = Pass.Proto $ reifyKnownTypeT @Abstracted initModel6
    --
initModel_reg' :: IRMonad m => PassManager m ()
initModel_reg' = registerLayerProto (typeVal' @Model) initModel2p
    --
    -- newtype LayerConsPass pass m = LayerConsPass (forall t. KnownType (Abstract t) => Event (NEW2 // Elem t) -> Pass (ElemScope pass t) m)
    --
    --
-- passT2 :: (Elem t, Definition t) -> Pass (ElemScope2 p t) m
-- passT2 = undefined

passT3 :: (cons -> Pass (ElemScope2 p t) m) -> Pass.PassTemplate (ElemScope2 p t) m
passT3 = Pass.template

-- passT4 :: forall p t m. Proxy t -> Pass.PassTemplate (ElemScope2 p t) m
-- passT4 = proxifyElemPass3

passT5 :: forall p t m. (Logging m, KnownType (Abstract t), Pass.DataLookup m, KnownElemPass p)
       => Pass.PassTemplate (ElemScope2 p t) m -> Initializer m (Template (Pass.DynPass3 m))
passT5 = Pass.initialize

passT6 :: forall p t m. (Logging m, KnownType (Abstract t), Pass.DataLookup m, KnownElemPass p)
       => Proxy t -> Pass.PassTemplate (ElemScope2 p t) m -> Pass.Describbed (Initializer m (Template (Pass.DynPass3 m)))
passT6 _ = Pass.describbed @(ElemScope2 p t) . passT5

passT7 :: forall p m. (Logging m, Pass.DataLookup m, KnownElemPass p)
       => (forall s. TypeReify (Abstracted s) => Pass.PassTemplate (ElemScope2 p (TypeRef s)) m) -> Pass.Proto (Pass.Describbed (Initializer m (Template (Pass.DynPass3 m))))
passT7 p = Pass.Proto $ reifyKnownTypeT @Abstracted (flip passT6 p)
    -- --


-- reifyKnownType :: forall r. (forall (s :: *). TypeReify s => TypeProxy s -> r) -> TypeRep -> r

--
-- ∀ k in (Keys (ElemScope2 p t)) => KeyMonad k m
--
-- class Monad m => KeyMonad key m n where
--     uncheckedLookupKey :: m (Maybe (Key n key))

-- passT5 :: forall p t m. (Logging m, KnownType (Abstract p), KnownType (Abstract t))
--        => Proxy t -> Initializer m (Template (Pass.DynPass3 m))
-- passT5 = fmap Pass.initialize (passT4 @p)


-- passT2 =

--
--
-- -------------------
-- -- === Model === --
-- -------------------
--
-- data InitModel
-- type instance Abstract InitModel = InitModel
-- type instance Inputs    (ElemScope InitModel t) = '[Layer (Abstract t) Model, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
-- type instance Outputs   (ElemScope InitModel t) = '[Layer (Abstract t) Model]
-- type instance Events    (ElemScope InitModel t) = '[]
-- type instance Preserves (ElemScope InitModel t) = '[]
--
-- initModel :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Pass (ElemScope InitModel t) m
-- initModel = do
--     (t, tdef) <- readAttr @WorkingElem
--     flip (writeLayer @Model) t tdef
--     debugLayerCreation' t "Model"
--
-- initModel_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => TypeRep -> Pass.DynPass m
-- initModel_dyn = reifyKnownTypeT @Abstracted $ Pass.compile <$> proxifyElemPass initModel
--
-- initModel_reg :: (IRMonad m, MonadIO m) => PassManager m ()
-- initModel_reg = registerLayer (typeVal' @Model) initModel_dyn
--

--
--
--
--
-- -----------------
-- -- === UID === --
-- -----------------
--
-- data InitUID
-- type instance Abstract InitUID = InitUID
-- type instance Inputs    (ElemScope InitUID t) = '[Layer (Abstract t) UID, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
-- type instance Outputs   (ElemScope InitUID t) = '[Layer (Abstract t) UID]
-- type instance Events    (ElemScope InitUID t) = '[]
-- type instance Preserves (ElemScope InitUID t) = '[]
--
-- initUID :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => STRefM m ID -> Pass (ElemScope InitUID t) m
-- initUID ref = do
--     (t, tdef) <- readAttr @WorkingElem
--     nuid <- Store.modifySTRef' ref (\i -> (i, succ i))
--     flip (writeLayer @UID) t nuid
--     debugLayerCreation t "UID" $ " (" <> show nuid <> ")"
--
--
-- initUID_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => m (TypeRep -> Pass.DynPass m)
-- initUID_dyn = do
--     r <- Store.newSTRef 0
--     return $ reifyKnownTypeT @Abstracted $ Pass.compile <$> proxifyElemPass (initUID r)
--
-- initUID_reg :: (IRMonad m, MonadIO m) => PassManager m ()
-- initUID_reg = registerLayer (typeVal' @UID) =<< initUID_dyn
--
--
--
-- -------------------
-- -- === Succs === --
-- -------------------
--
-- data InitSuccs
-- type instance Abstract InitSuccs = InitSuccs
-- type instance Inputs    (ElemScope InitSuccs t) = '[Layer (Abstract t) Succs, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
-- type instance Outputs   (ElemScope InitSuccs t) = '[Layer (Abstract t) Succs]
-- type instance Events    (ElemScope InitSuccs t) = '[]
-- type instance Preserves (ElemScope InitSuccs t) = '[]
--
-- initSuccs :: forall t m. (MonadIO m, IRMonad m, KnownType (Abstract t)) => Pass (ElemScope InitSuccs t) m
-- initSuccs = do
--     (t, _) <- readAttr @WorkingElem
--     flip (writeLayer @Succs) t mempty
--     debugLayerCreation' t "Succs"
--
-- initSuccs_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => TypeRep -> Pass.DynPass m
-- initSuccs_dyn = reifyKnownTypeT @Abstracted $ Pass.compile <$> proxifyElemPass initSuccs
--
-- initSuccs_reg :: (IRMonad m, MonadIO m) => PassManager m ()
-- initSuccs_reg = registerLayer (typeVal' @Succs) initSuccs_dyn
--
--
-- data WatchSuccs
-- type instance Abstract WatchSuccs = WatchSuccs
-- type instance Inputs    (ElemScope WatchSuccs t) = '[ExprLayer Succs, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
-- type instance Outputs   (ElemScope WatchSuccs t) = '[ExprLayer Succs]
-- type instance Events    (ElemScope WatchSuccs t) = '[]
-- type instance Preserves (ElemScope WatchSuccs t) = '[]
--
-- watchSuccs :: forall l m. (MonadIO m, IRMonad m) => Pass (ElemScope WatchSuccs (LINK' (Expr l))) m
-- watchSuccs = do
--     (t, (src, tgt)) <- readAttr @WorkingElem
--     debugElem t $ "New successor: " <> show (src ^. idx) <> " -> " <> show (tgt ^. idx)
--     modifyLayer_ @Succs (Set.insert $ unsafeGeneralize t) src
--
-- watchSuccs_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => Pass.DynPass m
-- watchSuccs_dyn = Pass.compile $ watchSuccs
--
-- data WatchRemoveEdge
-- type instance Abstract  WatchRemoveEdge               = WatchRemoveEdge
-- type instance Inputs    (ElemScope WatchRemoveEdge t) = '[ExprLayer Succs, ExprLinkLayer Model, Attr WorkingElem]
-- type instance Outputs   (ElemScope WatchRemoveEdge t) = '[ExprLayer Succs]
-- type instance Events    (ElemScope WatchRemoveEdge t) = '[]
-- type instance Preserves (ElemScope WatchRemoveEdge t) = '[]
--
-- watchRemoveEdge :: forall l m. (MonadIO m, IRMonad m) => Pass (ElemScope WatchRemoveEdge (LINK' (Expr l))) m
-- watchRemoveEdge = do
--     (t, _)     <- readAttr @WorkingElem
--     (src, tgt) <- readLayer @Model t
--     debugElem t $ "Delete successor: " <> show (src ^. idx) <> " -> " <> show (tgt ^. idx)
--     modifyLayer_ @Succs (Set.delete $ unsafeGeneralize t) src
--
-- data WatchRemoveNode
-- type instance Abstract  WatchRemoveNode               = WatchRemoveNode
-- type instance Inputs    (ElemScope WatchRemoveNode t) = '[ExprLayer Model, ExprLayer Type, Attr WorkingElem, ExprLinkNet]
-- type instance Outputs   (ElemScope WatchRemoveNode t) = '[ExprLayer Model, ExprLinkNet]
-- type instance Events    (ElemScope WatchRemoveNode t) = '[DELETE // LINK' EXPR]
-- type instance Preserves (ElemScope WatchRemoveNode t) = '[]
--
-- watchRemoveNode :: forall l m. (MonadIO m, IRMonad m, MonadPassManager m) => Pass (ElemScope WatchRemoveNode (EXPRESSION l)) m
-- watchRemoveNode = do
--     (e, _) <- readAttr @WorkingElem
--     inps   <- symbolFields (generalize e :: AnyExpr)
--     tp     <- readLayer @Type e
--     delete tp
--     mapM_ delete inps
--
--
-- ------------------
-- -- === Type === --
-- ------------------
--
-- consTypeLayer :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet], Emitter m (NEW // LINK' EXPR), Emitter m (NEW // EXPR))
--               => Store.STRefM m (Maybe (Expr Star)) -> Expr t -> m (LayerData Type (Expr t))
-- consTypeLayer ref self = do
--     top  <- unsafeRelayout <$> localTop ref
--     link top self
--
--
-- localTop :: (IRMonad m, Accessible ExprNet m, Emitter m (NEW // EXPR))
--          => Store.STRefM m (Maybe (Expr Star)) -> m (Expr Star)
-- localTop ref = Store.readSTRef ref >>= \case
--     Just t  -> return t
--     Nothing -> do
--         s <- reserveStar
--         Store.writeSTRef ref $ Just s
--         registerStar s
--         Store.writeSTRef ref Nothing
--         return s
--
--
-- data InitType
-- type instance Abstract InitType = InitType
-- type instance Inputs    (ElemScope InitType t) = '[Layer (Abstract t) Type, ExprNet, ExprLinkNet, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
-- type instance Outputs   (ElemScope InitType t) = '[Layer (Abstract t) Type, ExprNet, ExprLinkNet]
-- type instance Events    (ElemScope InitType t) = '[NEW // EXPR, NEW // LINK' EXPR]
-- type instance Preserves (ElemScope InitType t) = '[]
--
-- initType :: forall l m. (MonadIO m, IRMonad m, MonadPassManager m) => Store.STRefM m (Maybe (Expr Star)) -> Pass (ElemScope InitType (EXPRESSION l)) m
-- initType ref = do
--     (el, _) <- readAttr @WorkingElem
--     debugElem el "Type construction"
--     nested $ do
--         t <- consTypeLayer ref el
--         flip (writeLayer @Type) el t
--     debugLayerCreation' el "Type"
--
-- -- | Notice! This pass mimics signature needed by proto and the input TypeRep is not used
-- --   because it only works for Expressions
-- -- initType_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => TypeRep -> Pass.DynPass m
-- initType_dyn = do
--     r <- Store.newSTRef Nothing
--     return $ \ _ -> Pass.compile $ initType r
--
-- -- initType_reg :: (IRMonad m, MonadIO m) => PassManager m ()
-- initType_reg = registerLayer (typeVal' @Type) =<< initType_dyn
--
--
--
--




-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------

attachLayer priority l e = attachLayerIR l e >> attachLayerPM2 priority l e


-- FIXME [WD]: is the type ugly here?
-- runRegs :: _ => _
runRegs = do
    runElemRegs

    initModel_reg'
    attachLayer 0 (typeVal' @Model) (typeVal' @EXPR)

    -- attachLayer 0 (typeVal' @Model) (typeVal' @(LINK' EXPR))
    -- --
    -- initUID_reg
    -- attachLayer 5 (typeVal' @UID) (typeVal' @EXPR)
    -- attachLayer 5 (typeVal' @UID) (typeVal' @(LINK' EXPR))
    --
    -- initSuccs_reg
    -- attachLayer 5 (typeVal' @Succs) (typeVal' @EXPR)
    -- --
    -- initType_reg
    -- attachLayer 10 (typeVal' @Type) (typeVal' @EXPR)
    --
    -- addEventListener 100 (NEW    // LINK EXPR EXPR) watchSuccs
    -- addEventListener 100 (DELETE // LINK EXPR EXPR) watchRemoveEdge
    -- addEventListener 100 (DELETE // EXPR)           watchRemoveNode


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


source :: (MonadPass m, Reader LAYER (Layer (Abstract (Link a b)) Model) m) => Link a b -> m a
source = fmap fst . readLayer @Model ; {-# INLINE source #-}



type ExprLink a b = Link (Expr a) (Expr b)
-- strName :: _ => _
strName v = getName v >>= \n -> match' n >>= \ (Term.Sym_String s) -> return s



-- === KnownExpr === --

type KnownExpr l m = (MonadPass m, Readers LAYER '[ExprLayer Model, ExprLinkLayer Model] m) -- CheckAtomic (ExprHead l))

match' :: forall l m. KnownExpr l m => Expr l -> m (ExprHeadDef l)
match' = unsafeToExprTermDef @(ExprHead l)

modifyExprTerm :: forall l m. (KnownExpr l m, Writer LAYER (ExprLayer Model) m) => Expr l -> (ExprHeadDef l -> ExprHeadDef l) -> m ()
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
