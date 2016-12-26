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
import           Luna.Pass        (Pass, Preserves, Inputs, Outputs, Events, SubPass, Initializer, Template, DynPass3, ElemScope, KnownElemPass, elemPassDescription, genericDescription, genericDescription')
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




-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------
-- Layer passes


elemDesc (t :: t) = show (typeVal' @(Abstract t) :: TypeRep) <> " [" <> show (t ^. idx) <> "]"
layerCreated = ("Running pass " <>)

debugLayerCreation t layer s = withDebugBy (elemDesc t) (layerCreated layer <> s)
debugLayerCreation' t layer = debugLayerCreation t layer ""

--
-- debugElem :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> m ()
-- debugElem t = debugBy (show (typeVal' @(Abstract t) :: TypeRep) <> " [" <> show (t ^. idx) <> "]")
--
-- debugLayerCreation :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> Prelude.String -> m ()
-- debugLayerCreation t layer post = debugElem t $ layer <> " layer created" <> post
--
-- debugLayerCreation' :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> m ()
-- debugLayerCreation' t layer = debugLayerCreation t layer ""


proxify :: a -> Proxy a
proxify _ = Proxy




newtype GenLayerCons  p s = GenLayerCons (forall t m. (KnownType (Abstract t), MonadPassManager m, MonadPass m, s ~ PrimState m) => (Elem t, Definition t) -> Pass (ElemScope p t) m)
type    GenLayerConsM p m = GenLayerCons p (PrimState m)

newtype ExprLayerCons  p s = ExprLayerCons (forall l m. (MonadPassManager m, MonadPass m, s ~ PrimState m, GetBaseMonad m ~ GetBaseMonad (GetPassHandler m)) => (Expr l, Definition (EXPRESSION l)) -> Pass (ElemScope p (EXPRESSION l)) m)
type    ExprLayerConsM p m = ExprLayerCons p (PrimState m)

runGenLayerCons :: forall p m. (KnownType p, MonadPassManager m, MonadPass m) => GenLayerConsM p m -> forall t. KnownType (Abstract t) => (Elem t, Definition t) -> Pass (ElemScope p t) m
runGenLayerCons (GenLayerCons f) (t, tdef) = debugLayerCreation' t (show $ typeVal'_ @p) $ f (t, tdef)

runExprLayerCons :: forall p m. (KnownType p, MonadPassManager m, MonadPass m, GetBaseMonad m ~ GetBaseMonad (GetPassHandler m)) => ExprLayerConsM p m -> forall l. (Expr l, Definition (EXPRESSION l)) -> Pass (ElemScope p (EXPRESSION l)) m
runExprLayerCons (ExprLayerCons f) (t, tdef) = debugLayerCreation' t (show $ typeVal'_ @p) $ f (t, tdef)


registerGenLayer :: (MonadPassManager m, KnownElemPass p, KnownType p) => LayerRep -> GenLayerConsM p (GetBaseMonad m) -> m ()
registerGenLayer l p = registerLayerProto l $ prepareProto $ Pass.template $ runGenLayerCons p ; {-# INLINE registerGenLayer #-}


registerGenLayerM :: (MonadPassManager m, KnownElemPass p, KnownType p) => LayerRep -> m (GenLayerConsM p (GetBaseMonad m)) -> m ()
registerGenLayerM l p = registerGenLayer l =<< p ; {-# INLINE registerGenLayerM #-}


registerExprLayer :: forall p l m. (MonadPassManager m, KnownElemPass p, KnownType p) => LayerRep -> ExprLayerConsM p (GetBaseMonad m) -> m ()
registerExprLayer l p = registerLayerProto l $ Pass.Proto $ \_ -> Pass.describbed @(ElemScope p (EXPRESSION l)) . Pass.initialize $ Pass.template $ runExprLayerCons p ; {-# INLINE registerExprLayer #-}

registerExprLayerM :: (MonadPassManager m, KnownElemPass p, KnownType p) => LayerRep -> m (ExprLayerConsM p (GetBaseMonad m)) -> m ()
registerExprLayerM l p = registerExprLayer l =<< p ; {-# INLINE registerExprLayerM #-}



prepareProto :: forall p m. (Logging m, Pass.DataLookup m, KnownElemPass p) => (forall s. TypeReify (Abstracted s) => Pass.PassTemplate (ElemScope p (TypeRef s)) m) -> Pass.Proto (Pass.Describbed (Initializer m (Template (Pass.DynPass3 m))))
prepareProto p = Pass.Proto $ reifyKnownTypeT @Abstracted (prepareProto' p) where
    prepareProto' :: forall p t m. (Logging m, KnownType (Abstract t), Pass.DataLookup m, KnownElemPass p) => Pass.PassTemplate (ElemScope p t) m -> Proxy t -> Pass.Describbed (Initializer m (Template (Pass.DynPass3 m)))
    prepareProto' = const . Pass.describbed @(ElemScope p t) . Pass.initialize



-------------------
-- === Model === --
-------------------


data InitModel
type instance Abstract InitModel = InitModel
type instance Inputs  NET   (ElemScope InitModel t) = '[]
type instance Outputs NET   (ElemScope InitModel t) = '[]
type instance Inputs  LAYER (ElemScope InitModel t) = '[Layer (Abstract t) Model] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs LAYER (ElemScope InitModel t) = '[Layer (Abstract t) Model] -- FIXME[bug: unnecessary inputs needed]
type instance Inputs  ATTR  (ElemScope InitModel t) = '[]
type instance Outputs ATTR  (ElemScope InitModel t) = '[]
type instance Inputs  EVENT (ElemScope InitModel t) = '[]
type instance Outputs EVENT (ElemScope InitModel t) = '[]
type instance Preserves     (ElemScope InitModel t) = '[]
instance KnownElemPass InitModel where
    elemPassDescription = genericDescription' . proxify

initModel :: GenLayerCons InitModel s
initModel = GenLayerCons $ uncurry $ flip $ writeLayer @Model ; {-# INLINE initModel #-}



-----------------
-- === UID === --
-----------------

data InitUID
type instance Abstract InitUID = InitUID
type instance Inputs  NET   (ElemScope InitUID t) = '[]
type instance Outputs NET   (ElemScope InitUID t) = '[]
type instance Inputs  LAYER (ElemScope InitUID t) = '[Layer (Abstract t) UID] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs LAYER (ElemScope InitUID t) = '[Layer (Abstract t) UID] -- FIXME[bug: unnecessary inputs needed]
type instance Inputs  ATTR  (ElemScope InitUID t) = '[]
type instance Outputs ATTR  (ElemScope InitUID t) = '[]
type instance Inputs  EVENT (ElemScope InitUID t) = '[]
type instance Outputs EVENT (ElemScope InitUID t) = '[]
type instance Preserves     (ElemScope InitUID t) = '[]
instance KnownElemPass InitUID where
    elemPassDescription = genericDescription' . proxify


initUID :: PrimMonad m => m (GenLayerConsM InitUID m)
initUID = do
    ref <- Store.newSTRef (def :: ID)
    return $ GenLayerCons $ \(t, tdef) -> do --  (" (" <> show nuid <> ")")
        nuid <- Store.modifySTRef' ref (\i -> (i, succ i))
        writeLayer @UID nuid t


-------------------
-- === Succs === --
-------------------

data InitSuccs
type instance Abstract InitSuccs = InitSuccs
type instance Inputs  NET   (ElemScope InitSuccs t) = '[]
type instance Outputs NET   (ElemScope InitSuccs t) = '[]
type instance Inputs  LAYER (ElemScope InitSuccs t) = '[Layer (Abstract t) Succs] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs LAYER (ElemScope InitSuccs t) = '[Layer (Abstract t) Succs] -- FIXME[bug: unnecessary inputs needed]
type instance Inputs  ATTR  (ElemScope InitSuccs t) = '[]
type instance Outputs ATTR  (ElemScope InitSuccs t) = '[]
type instance Inputs  EVENT (ElemScope InitSuccs t) = '[]
type instance Outputs EVENT (ElemScope InitSuccs t) = '[]
type instance Preserves     (ElemScope InitSuccs t) = '[]
instance KnownElemPass InitSuccs where
    elemPassDescription = genericDescription' . proxify


initSuccs :: GenLayerCons InitSuccs s
initSuccs = GenLayerCons $ \(t, _) -> writeLayer @Succs mempty t ; {-# INLINE initSuccs #-}


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
------------------
-- === Type === --
------------------

consTypeLayer :: (MonadPass m, Editors NET '[EXPR, LINK' EXPR] m, Emitter2 m (NEW2 // LINK' EXPR), Emitter2 m (NEW2 // EXPR))
              => Store.STRefM m (Maybe (Expr Star)) -> Expr t -> m (LayerData Type (Expr t))
consTypeLayer ref self = do
    top  <- unsafeRelayout <$> localTop ref
    link top self


localTop :: (MonadPass m, Editor NET EXPR m, Emitter2 m (NEW2 // EXPR))
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
type instance Inputs  NET   (ElemScope InitType t) = '[EXPR, LINK' EXPR]
type instance Outputs NET   (ElemScope InitType t) = '[]
type instance Inputs  LAYER (ElemScope InitType t) = '[Layer (Abstract t) Type] -- FIXME[bug: unnecessary inputs needed]
type instance Outputs LAYER (ElemScope InitType t) = '[Layer (Abstract t) Type] -- FIXME[bug: unnecessary inputs needed]
type instance Inputs  ATTR  (ElemScope InitType t) = '[]
type instance Outputs ATTR  (ElemScope InitType t) = '[]
type instance Inputs  EVENT (ElemScope InitType t) = '[]
type instance Outputs EVENT (ElemScope InitType t) = '[NEW2 // EXPR, NEW2 // LINK' EXPR]
type instance Preserves     (ElemScope InitType t) = '[]
instance KnownElemPass InitType where
    elemPassDescription = genericDescription' . proxify


-- initSuccs :: GenLayerCons InitType s
-- initSuccs = GenLayerCons $ \(t, _) -> writeLayer @Succs mempty t ; {-# INLINE initSuccs #-}

--
--
-- data InitType
-- type instance Abstract InitType = InitType
-- type instance Inputs    (ElemScope InitType t) = '[Layer (Abstract t) Type, ExprNet, ExprLinkNet, Attr WorkingElem] -- FIXME[bug: unnecessary inputs needed]
-- type instance Outputs   (ElemScope InitType t) = '[Layer (Abstract t) Type, ExprNet, ExprLinkNet]
-- type instance Events    (ElemScope InitType t) = '[NEW // EXPR, NEW // LINK' EXPR]
-- type instance Preserves (ElemScope InitType t) = '[]

-- initType :: forall l m. (MonadIO m, MonadPassManager m) => Store.STRefM m (Maybe (Expr Star)) -> (Elem (EXPRESSION l), Definition (EXPRESSION l)) -> Pass (ElemScope InitType (EXPRESSION l)) m
-- initType :: forall l m. (MonadIO m, MonadPassManager m) => Store.STRefM m (Maybe (Expr Star)) -> ExprLayerConsM InitType m
initType :: PrimMonad m => m (ExprLayerConsM InitType m)
initType = do
    ref <- Store.newSTRef (Nothing :: Maybe (Expr Star))
    return $ ExprLayerCons $ \(el, _) -> do
        -- debugElem el "Type construction"
        t <- consTypeLayer ref el
        flip (writeLayer @Type) el t
        -- debugLayerCreation' el "Type"
--

-- -- | Notice! This pass mimics signature needed by proto and the input TypeRep is not used
-- --   because it only works for Expressions
-- -- initType_dyn :: (IRMonad m, MonadIO m, MonadPassManager m) => TypeRep -> Pass.DynPass m
-- initType_dyn = do
--     r <- Store.newSTRef Nothing
--     return $ \ _ -> Pass.compile $ initType r
--
-- -- initType_reg :: (IRMonad m, MonadIO m) => PassManager m ()
-- initType_reg = registerGenLayer (typeVal' @Type) =<< initType_dyn
--
--
--
--



-- ff = initUID' <$> Store.newSTRef 0
-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------

attachLayer priority l e = attachLayerIR l e >> attachLayerPM2 priority l e


-- FIXME [WD]: is the type ugly here?
-- runRegs :: _ => _
runRegs = do
    runElemRegs

    -- f <- ff
    registerGenLayer  (typeVal' @Model) initModel
    registerGenLayerM (typeVal' @UID)   initUID
    registerGenLayer  (typeVal' @Succs) initSuccs

    registerExprLayerM (typeVal' @Type) initType

    attachLayer 0 (typeVal' @Model) (typeVal' @EXPR)
    attachLayer 5 (typeVal' @UID)   (typeVal' @EXPR)
    attachLayer 5 (typeVal' @Succs) (typeVal' @EXPR)

    attachLayer 5 (typeVal' @UID)   (typeVal' @(LINK' EXPR))

    attachLayer 10 (typeVal' @Type) (typeVal' @EXPR)


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
