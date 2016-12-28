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
import           Luna.Pass        (Pass, Preserves, Inputs, Outputs, Events, SubPass, Uninitialized, Template, DynPass, ElemScope, KnownElemPass, elemPassDescription, genericDescription, genericDescription')
import Data.TypeVal
import Data.Event (type (//))
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




type instance RefData Attr a _ = a





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


-- m (m [Template (DynPass (GetPassManager m))])




-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------
-- Layer passes


elemDesc (t :: t) = show (typeVal' @(Abstract t) :: TypeDesc) <> " [" <> show (t ^. idx) <> "]"
layerCreated = ("Running pass " <>)

debugLayerCreation t layer s = withDebugBy ("Pass [" <> layer <> "]") ("Running on " <> elemDesc t)
debugLayerCreation' t layer = debugLayerCreation t layer ""

--
-- debugElem :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> m ()
-- debugElem t = debugBy (show (typeVal' @(Abstract t) :: TypeDesc) <> " [" <> show (t ^. idx) <> "]")
--
-- debugLayerCreation :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> Prelude.String -> m ()
-- debugLayerCreation t layer post = debugElem t $ layer <> " layer created" <> post
--
-- debugLayerCreation' :: forall t m. (IsIdx t, KnownType (Abstract t)) => Logging m => t -> Prelude.String -> m ()
-- debugLayerCreation' t layer = debugLayerCreation t layer ""


proxify :: a -> Proxy a
proxify _ = Proxy


newtype LayerCons  p t s = LayerCons (forall m. (MonadPassManager m, MonadRef m, s ~ PrimState m) => (t, Definition t) -> Pass (ElemScope p t) m)
type    LayerConsM p t m = LayerCons p t (PrimState m)

newtype GenLayerCons  p s = GenLayerCons (forall t m. (KnownType (Abstract t), MonadPassManager m, MonadRef m, s ~ PrimState m) => (Elem t, Definition t) -> Pass (ElemScope p (Elem t)) m)
type    GenLayerConsM p m = GenLayerCons p (PrimState m)

newtype ExprLayerCons  p s = ExprLayerCons (forall l m. (MonadPassManager m, MonadRef m, s ~ PrimState m) => (Expr l, Definition (Expr l)) -> Pass (ElemScope p (Expr l)) m)
type    ExprLayerConsM p m = ExprLayerCons p (PrimState m)

runGenLayerCons :: forall p m. (KnownType p, MonadPassManager m, MonadRef m) => GenLayerConsM p m -> forall t. KnownType (Abstract t) => (Elem t, Definition t) -> Pass (ElemScope p (Elem t)) m
runGenLayerCons (GenLayerCons f) (t, tdef) = debugLayerCreation' t (show $ typeVal'_ @p) $ f (t, tdef)

runExprLayerCons :: forall p m. (KnownType p, MonadPassManager m, MonadRef m) => ExprLayerConsM p m -> forall l. (Expr l, Definition (Expr l)) -> Pass (ElemScope p (Expr l)) m
runExprLayerCons (ExprLayerCons f) (t, tdef) = debugLayerCreation' t (show $ typeVal'_ @p) $ f (t, tdef)


registerGenLayer :: (MonadPassManager m, MonadPassManager (GetRefHandler m), Pass.DataLookup (GetRefHandler m), KnownElemPass p, KnownType p) => LayerRep -> GenLayerConsM p (GetRefHandler m) -> m ()
registerGenLayer l p = registerLayerProto l $ prepareProto $ Pass.template $ runGenLayerCons p ; {-# INLINE registerGenLayer #-}


registerGenLayerM :: (MonadPassManager m, KnownElemPass p, KnownType p, MonadPassManager (GetRefHandler m), Pass.DataLookup (GetRefHandler m)) => LayerRep -> m (GenLayerConsM p (GetRefHandler m)) -> m ()
registerGenLayerM l p = registerGenLayer l =<< p ; {-# INLINE registerGenLayerM #-}


registerExprLayer :: forall p l m. (MonadPassManager m, KnownElemPass p, KnownType p, MonadPassManager (GetRefHandler m), Pass.DataLookup (GetRefHandler m)) => LayerRep -> ExprLayerConsM p (GetRefHandler m) -> m ()
registerExprLayer l p = registerLayerProto l $ Pass.Proto $ \_ -> Pass.describbed @(ElemScope p (Expr l)) . Pass.compileTemplate $ Pass.template $ runExprLayerCons p ; {-# INLINE registerExprLayer #-}

registerExprLayerM :: (MonadPassManager m, KnownElemPass p, KnownType p, MonadPassManager (GetRefHandler m), Pass.DataLookup (GetRefHandler m)) => LayerRep -> m (ExprLayerConsM p (GetRefHandler m)) -> m ()
registerExprLayerM l p = registerExprLayer l =<< p ; {-# INLINE registerExprLayerM #-}



prepareProto :: forall p m. (Logging m, Pass.DataLookup m, KnownElemPass p) => (forall s. TypeReify (Abstracted s) => Template (Pass (ElemScope p (Elem (TypeRef s))) m)) -> Pass.Proto (Pass.Describbed (Uninitialized m (Template (DynPass m))))
prepareProto p = Pass.Proto $ reifyKnownTypeT @Abstracted (prepareProto' p) . (descTypeRep %~ head . typeRepArgs) {- we take type args here, cause we need only `t` instead of `Elem t` -} where
    prepareProto' :: forall p t m. (Logging m, KnownType (Abstract t), Pass.DataLookup m, KnownElemPass p) => Template (Pass (ElemScope p (Elem t)) m) -> Proxy t -> Pass.Describbed (Uninitialized m (Template (DynPass m)))
    prepareProto' = const . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate



-------------------
-- === Model === --
-------------------


data InitModel
instance TypeShow InitModel
type instance Abstract InitModel = InitModel
type instance Inputs  Net   (ElemScope InitModel t) = '[]
type instance Outputs Net   (ElemScope InitModel t) = '[]
type instance Inputs  Layer (ElemScope InitModel t) = '[]
type instance Outputs Layer (ElemScope InitModel t) = '[Abstract t // Model]
type instance Inputs  Attr  (ElemScope InitModel t) = '[]
type instance Outputs Attr  (ElemScope InitModel t) = '[]
type instance Inputs  Event (ElemScope InitModel t) = '[]
type instance Outputs Event (ElemScope InitModel t) = '[]
type instance Preserves     (ElemScope InitModel t) = '[]
instance KnownElemPass InitModel where
    elemPassDescription = genericDescription' . proxify

initModel :: GenLayerCons InitModel s
initModel = GenLayerCons $ uncurry $ flip $ writeLayer @Model ; {-# INLINE initModel #-}



-----------------
-- === UID === --
-----------------

data InitUID
instance TypeShow InitUID
type instance Abstract InitUID = InitUID
type instance Inputs  Net   (ElemScope InitUID t) = '[]
type instance Outputs Net   (ElemScope InitUID t) = '[]
type instance Inputs  Layer (ElemScope InitUID t) = '[]
type instance Outputs Layer (ElemScope InitUID t) = '[Abstract t // UID]
type instance Inputs  Attr  (ElemScope InitUID t) = '[]
type instance Outputs Attr  (ElemScope InitUID t) = '[]
type instance Inputs  Event (ElemScope InitUID t) = '[]
type instance Outputs Event (ElemScope InitUID t) = '[]
type instance Preserves     (ElemScope InitUID t) = '[]
instance KnownElemPass InitUID where
    elemPassDescription = genericDescription' . proxify


initUID :: PrimMonad m => m (GenLayerConsM InitUID m)
initUID = do
    ref <- Store.newSTRef (def :: ID)
    return $ GenLayerCons $ \(t, tdef) -> do
        nuid <- Store.modifySTRef' ref (\i -> (i, succ i))
        writeLayer @UID nuid t
{-# INLINE initUID #-}


-------------------
-- === Succs === --
-------------------

data InitSuccs
instance TypeShow InitSuccs
type instance Abstract InitSuccs = InitSuccs
type instance Inputs  Net   (ElemScope InitSuccs t) = '[]
type instance Outputs Net   (ElemScope InitSuccs t) = '[]
type instance Inputs  Layer (ElemScope InitSuccs t) = '[]
type instance Outputs Layer (ElemScope InitSuccs t) = '[Abstract t // Succs]
type instance Inputs  Attr  (ElemScope InitSuccs t) = '[]
type instance Outputs Attr  (ElemScope InitSuccs t) = '[]
type instance Inputs  Event (ElemScope InitSuccs t) = '[]
type instance Outputs Event (ElemScope InitSuccs t) = '[]
type instance Preserves     (ElemScope InitSuccs t) = '[]
instance KnownElemPass InitSuccs where
    elemPassDescription = genericDescription' . proxify


initSuccs :: GenLayerCons InitSuccs s
initSuccs = GenLayerCons $ \(t, _) -> writeLayer @Succs mempty t ; {-# INLINE initSuccs #-}


data WatchSuccs
instance TypeShow WatchSuccs
type instance Abstract WatchSuccs = WatchSuccs
type instance Inputs  Net   (ElemScope WatchSuccs t) = '[]
type instance Outputs Net   (ElemScope WatchSuccs t) = '[]
type instance Inputs  Layer (ElemScope WatchSuccs t) = '[AnyExpr // Succs]
type instance Outputs Layer (ElemScope WatchSuccs t) = '[AnyExpr // Succs]
type instance Inputs  Attr  (ElemScope WatchSuccs t) = '[]
type instance Outputs Attr  (ElemScope WatchSuccs t) = '[]
type instance Inputs  Event (ElemScope WatchSuccs t) = '[]
type instance Outputs Event (ElemScope WatchSuccs t) = '[]
type instance Preserves     (ElemScope WatchSuccs t) = '[]
instance KnownElemPass WatchSuccs where
    elemPassDescription = genericDescription' . proxify

-- newtype LayerCons  p t s = LayerCons (forall m. (MonadPassManager m, MonadRef m, s ~ PrimState m) => (t, Definition t) -> Pass (ElemScope p t) m)

-- watchSuccs :: forall l m. (MonadIO m, MonadIR m) => Pass (ElemScope WatchSuccs (Link' (Expr l))) m
watchSuccs :: LayerCons WatchSuccs (Link' (Expr l)) s
watchSuccs = LayerCons $ \(t, (src, tgt)) -> do
    -- debugElem t $ "New successor: " <> show (src ^. idx) <> " -> " <> show (tgt ^. idx)
    modifyLayer_ @Succs (Set.insert $ unsafeGeneralize t) src
--
-- watchSuccs_dyn :: (MonadIR m, MonadIO m, MonadPassManager m) => DynPass m
-- watchSuccs_dyn = Pass.compile $ watchSuccs
--
-- data WatchRemoveEdge
-- type instance Abstract  WatchRemoveEdge               = WatchRemoveEdge
-- type instance Inputs    (ElemScope WatchRemoveEdge t) = '[ExprLayer Succs, ExprLinkLayer Model, Attr WorkingElem]
-- type instance Outputs   (ElemScope WatchRemoveEdge t) = '[ExprLayer Succs]
-- type instance Events    (ElemScope WatchRemoveEdge t) = '[]
-- type instance Preserves (ElemScope WatchRemoveEdge t) = '[]
--
-- watchRemoveEdge :: forall l m. (MonadIO m, MonadIR m) => Pass (ElemScope WatchRemoveEdge (Link' (Expr l))) m
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
-- type instance Events    (ElemScope WatchRemoveNode t) = '[DELETE // Link' AnyExpr]
-- type instance Preserves (ElemScope WatchRemoveNode t) = '[]
--
-- watchRemoveNode :: forall l m. (MonadIO m, MonadIR m, MonadPassManager m) => Pass (ElemScope WatchRemoveNode (EXPR l)) m
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

consTypeLayer :: (MonadRef m, Writers Net '[AnyExpr, Link' AnyExpr] m, Emitters '[New // Link' AnyExpr, New // AnyExpr] m)
              => Store.STRefM m (Maybe (Expr Star)) -> Expr t -> m (LayerData Type (Expr t))
consTypeLayer ref self = (`link` self) =<< unsafeRelayout <$> localTop ref ; {-# INLINE consTypeLayer #-}


localTop :: (MonadRef m, Writer Net AnyExpr m, Emitter (New // AnyExpr) m)
         => Store.STRefM m (Maybe (Expr Star)) -> m (Expr Star)
localTop ref = Store.readSTRef ref >>= \case
    Just t  -> return t
    Nothing -> do
        s <- reserveStar
        Store.writeSTRef ref $ Just s
        registerStar s
        Store.writeSTRef ref Nothing
        return s
{-# INLINE localTop #-}


data InitType
instance TypeShow InitType
type instance Abstract InitType = InitType
type instance Inputs  Net   (ElemScope InitType t) = '[]
type instance Outputs Net   (ElemScope InitType t) = '[AnyExpr, Link' AnyExpr]
type instance Inputs  Layer (ElemScope InitType t) = '[]
type instance Outputs Layer (ElemScope InitType t) = '[Abstract t // Type]
type instance Inputs  Attr  (ElemScope InitType t) = '[]
type instance Outputs Attr  (ElemScope InitType t) = '[]
type instance Inputs  Event (ElemScope InitType t) = '[]
type instance Outputs Event (ElemScope InitType t) = '[New // AnyExpr, New // Link' AnyExpr]
type instance Preserves     (ElemScope InitType t) = '[]
instance KnownElemPass InitType where
    elemPassDescription = genericDescription' . proxify

initType :: PrimMonad m => m (ExprLayerConsM InitType m)
initType = do
    ref <- Store.newSTRef (Nothing :: Maybe (Expr Star))
    return $ ExprLayerCons $ \(el, _) -> do
        t <- consTypeLayer ref el
        flip (writeLayer @Type) el t
{-# INLINE initType #-}





-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------


-- FIXME [WD]: is the type ugly here?
-- runRegs :: _ => _
runRegs = do
    runElemRegs

    -- f <- ff
    registerGenLayer  (typeVal' @Model) initModel
    registerGenLayerM (typeVal' @UID)   initUID
    registerGenLayer  (typeVal' @Succs) initSuccs

    registerExprLayerM (typeVal' @Type) initType

    attachLayer 0 (typeVal' @Model) (typeVal' @AnyExpr)
    attachLayer 5 (typeVal' @UID)   (typeVal' @AnyExpr)
    attachLayer 5 (typeVal' @Succs) (typeVal' @AnyExpr)

    attachLayer 5 (typeVal' @UID)   (typeVal' @(Link' AnyExpr))

    attachLayer 10 (typeVal' @Type) (typeVal' @AnyExpr)


    -- attachLayer 0 (typeVal' @Model) (typeVal' @(Link' AnyExpr))
    -- --
    -- initUID_reg
    -- attachLayer 5 (typeVal' @UID) (typeVal' @AnyExpr)
    -- attachLayer 5 (typeVal' @UID) (typeVal' @(Link' AnyExpr))
    --
    -- initSuccs_reg
    -- attachLayer 5 (typeVal' @Succs) (typeVal' @AnyExpr)
    -- --
    -- initType_reg
    -- attachLayer 10 (typeVal' @Type) (typeVal' @AnyExpr)
    --
    -- addEventListener 100 (New    // LINK AnyExpr AnyExpr) watchSuccs
    -- addEventListener 100 (DELETE // LINK AnyExpr AnyExpr) watchRemoveEdge
    -- addEventListener 100 (DELETE // AnyExpr)           watchRemoveNode


-- === Elem reg defs === --

runElemRegs :: MonadIR m => m ()
runElemRegs = sequence_ [elemReg1, elemReg2, elemReg3]

elemReg1 :: MonadIR m => m ()
elemReg1 = registerElem @AnyExpr

elemReg2 :: MonadIR m => m ()
elemReg2 = registerElem @(Link' AnyExpr)

elemReg3 :: MonadIR m => m ()
elemReg3 = registerElem @(GROUP AnyExpr)


-- === Layer reg defs === --

layerRegs :: MonadIR m => [m ()]
layerRegs = [] -- [layerReg1, layerReg2, layerReg3, layerReg4]

runLayerRegs :: MonadIR m => m ()
runLayerRegs = sequence_ layerRegs





----------------------------------
----------------------------------
----------------------------------


source :: (MonadRef m, Reader Layer (Abstract (Link a b) // Model) m) => Link a b -> m a
source = fmap fst . readLayer @Model ; {-# INLINE source #-}



type ExprLink a b = Link (Expr a) (Expr b)
-- strName :: _ => _
strName v = getName v >>= \n -> match' n >>= \ (Term.Sym_String s) -> return s



-- === KnownExpr === --

type KnownExpr l m = (MonadRef m, Readers Layer '[AnyExpr // Model, Link' AnyExpr // Model] m) -- CheckAtomic (ExprHead l))

match' :: forall l m. KnownExpr l m => Expr l -> m (ExprHeadDef l)
match' = unsafeToExprTermDef @(ExprHead l)

modifyExprTerm :: forall l m. (KnownExpr l m, Writer Layer (AnyExpr // Model) m) => Expr l -> (ExprHeadDef l -> ExprHeadDef l) -> m ()
modifyExprTerm = unsafeModifyExprTermDef @(ExprHead l)

getSource :: KnownExpr l m => Lens' (ExprHeadDef l) (ExprLink a b) -> Expr l -> m (Expr a)
getSource f v = match' v >>= source . view f ; {-# INLINE getSource #-}


-- === KnownName === --

type       KnownName l m = (KnownExpr l m, HasName (ExprHeadDef l))
getName :: KnownName l m => Expr l -> m (Expr (Sub NAME l))
getName = getSource name ; {-# INLINE getName #-}







type family Head a

type instance Access AnyExpr (ENT e _ _) = e
type instance Access AnyExpr (E   e    ) = e
type instance Head (Atomic a) = Atomic a

type ExprHead l = Head (l # AnyExpr)
type ExprHeadDef l = ExprTermDef (ExprHead l) (Expr l)



---------- TRASH
------ TO BE DELETED WHEN POSSIBLE

instance MonadLogging m => MonadLogging (DepState.StateT a b m)
