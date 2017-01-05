{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.IR.ToRefactor where

import Luna.Prelude hiding (String, log, nested)
import qualified Luna.Prelude as Prelude
import Control.Arrow ((&&&))
import Data.STRef    (STRef)

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
import           Luna.Pass        (Pass, Preserves, Inputs, Outputs, Events, SubPass, Uninitialized, Template, DynPass, ElemScope, KnownElemPass, elemPassDescription, genericDescription, genericDescriptionP)
import Data.TypeDesc
import Data.Event (type (//), Tag(Tag), Emitter, PayloadData, Event, Emitters)
import qualified Data.Set as Set
import Luna.IR.Internal.LayerStore (STRef, STRefM)
import Luna.IR.Expr
import Unsafe.Coerce (unsafeCoerce)
import Luna.Pass.Manager as PM
import qualified Data.Event as Event
import System.Log
import qualified Control.Monad.State.Dependent.Old as DepState

import qualified GHC.Prim as Prim

import Data.Reflection (Reifies)
import Luna.Pass.Sugar

import Type.Any (AnyType)

---------------------------------------
-- Some important utils


type GraphElems = '[AnyExpr, AnyExprLink]













-----------------------------------------------



instance {-# OVERLAPPABLE #-} TypePretty (ElemScope p t) where formatType [p,_] = [p]








data Abstracted a
type instance Abstract (TypeRef s) = TypeRef (Abstracted s)




-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------
-- Layer passes

data Init a



newtype SubPassDef p m a = SubPassDef (m a) deriving (Functor, Applicative, Monad)
type       PassDef p m   = SubPassDef p m ()
makeWrapped ''SubPassDef

subPassDef :: m a -> SubPassDef p m a
subPassDef = wrap' ; {-# INLINE subPassDef #-}

runSubPassdef :: SubPassDef p m a -> m a
runSubPassdef = unwrap' ; {-# INLINE runSubPassdef #-}


newtype Listener p e t m = Listener (PayloadData (e // t) -> PassDef p m)
makeWrapped ''Listener

newtype Listener2 e t m = Listener2 (PayloadData (e // t) -> m ())
makeWrapped ''Listener2

type Initializer p t m = Listener (Init p) New t m
type ElemInitializer  p t m = Initializer p (Elem t) m

listener :: (PayloadData (e // t) -> m ()) -> Listener p e t m
listener = wrap' . fmap subPassDef ; {-# INLINE listener #-}

runListener :: Listener p e t m -> (PayloadData (e // t) -> m ())
runListener = fmap runSubPassdef . unwrap' ; {-# INLINE runListener #-}


listener2 :: (PayloadData (e // t) -> m ()) -> Listener2 e t m
listener2 = wrap' ; {-# INLINE listener2 #-}

runListener2 :: Listener2 e t m -> (PayloadData (e // t) -> m ())
runListener2 = unwrap' ; {-# INLINE runListener2 #-}




compileListener :: forall p e t m. (KnownType (Abstract t), KnownElemPass p, Pass.PassInit (ElemScope p (Elem t)) m, Event.KnownTag e)
                => Listener p e (Elem t) (SubPass (ElemScope p t) m) -> Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m))))
compileListener = Event.Tagged (Event.fromPath @e) . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate . Pass.template . runListener


compileListener' :: forall p e t m. (KnownType (Abstract t), KnownElemPass p, Pass.PassInit (ElemScope p (Elem t)) m, Event.KnownTag e)
                => Listener p e (Elem t) (SubPass (ElemScope p t) m) -> Pass.Describbed (Uninitialized m (Template (DynPass m)))
compileListener' = Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate . Pass.template . runListener




unsafeRegisterSpecLayer :: forall p e t m. (MonadPassManager m, KnownElemPass (Init p), KnownType p, KnownType (Abstract t), Event.KnownTag e)
                        => Listener (Init p) e (Elem t) (SubPass (ElemScope (Init p) t) (GetRefHandler m)) -> m ()
unsafeRegisterSpecLayer p = registerGenericElemEventListener (getTypeDesc @p) (getTypeDesc @(Init p)) $ Pass.constProto $ compileListener p ; {-# INLINE unsafeRegisterSpecLayer #-}

registerExprLayer :: forall p e m. (MonadPassManager m, KnownElemPass (Init p), KnownType p, Event.KnownTag e)
                  => (forall t. Listener (Init p) e (Expr t) (SubPass (ElemScope (Init p) (EXPR t)) (GetRefHandler m))) -> m ()
registerExprLayer = unsafeRegisterSpecLayer @p ; {-# INLINE registerExprLayer #-}



addLayerGenericListener :: forall p e m. (MonadPassManager m, KnownElemPass (Init p), KnownType p)
                  => (forall t. KnownType (Abstract t) => Listener (Init p) e (Elem t) (SubPass (ElemScope (Init p) t) (GetRefHandler m))) -> m ()
addLayerGenericListener p = registerGenericElemEventListener (getTypeDesc @p) (getTypeDesc @(Init p)) $ prepareProto $ Pass.template $ runListener p







addGenericElemEventListener :: forall l p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e)
                       => (forall t. KnownType (Abstract t) => Listener2 e (Elem t) (SubPass (ElemScope p t) (GetRefHandler m))) -> m ()
addGenericElemEventListener p = registerGenericElemEventListener (getTypeDesc @l) (getTypeDesc @p) $ prepareProto2 @e $ Pass.template $ runListener2 p

addSpecificElemEventListener :: forall l t p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e, KnownType (Abstract t))
                       => Listener2 e (Elem t) (SubPass (ElemScope p t) (GetRefHandler m)) -> m ()
addSpecificElemEventListener p = registerSpecificElemEventListener (getTypeDesc @(Abstract (Elem t))) (getTypeDesc @l) (getTypeDesc @p) $ compileListener2 p



prepareProto2 :: forall e p m. (Logging m, Pass.PassConstruction m, KnownElemPass p, Event.KnownTag e) => (forall s. TypeReify (Abstracted s) => Template (Pass (ElemScope p (TypeRef s)) m)) -> Pass.Proto (Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m)))))
prepareProto2 p = Pass.Proto $ reifyKnownTypeT @Abstracted (prepareProto2' @e p) . (head . view subDescs) {- we take type args here, cause we need only `t` instead of `Elem t` -} where
    prepareProto2' :: forall e p t m. (Logging m, KnownType (Abstract t), Pass.PassConstruction m, KnownElemPass p, Event.KnownTag e) => Template (Pass (ElemScope p t) m) -> Proxy t -> Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m))))
    prepareProto2' = const . Event.Tagged (Event.fromPath @e) . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate


unsafeRegisterSpecLayer2 :: forall l p e t m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, KnownType (Abstract t), Event.KnownTag e)
                        => Listener2 e (Elem t) (SubPass (ElemScope p t) (GetRefHandler m)) -> m ()
unsafeRegisterSpecLayer2 p = registerGenericElemEventListener (getTypeDesc @l) (getTypeDesc @p) $ Pass.constProto $ compileListener2 p ; {-# INLINE unsafeRegisterSpecLayer2 #-}

compileListener2 :: forall p e t m. (KnownType (Abstract t), KnownElemPass p, Pass.PassInit (ElemScope p (Elem t)) m, Event.KnownTag e)
                => Listener2 e (Elem t) (SubPass (ElemScope p t) m) -> Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m))))
compileListener2 = Event.Tagged (Event.fromPath @e) . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate . Pass.template . runListener2





prepareProto :: forall p m. (Logging m, Pass.PassConstruction m, KnownElemPass p) => (forall s. TypeReify (Abstracted s) => Template (Pass (ElemScope p (TypeRef s)) m)) -> Pass.Proto (Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m)))))
prepareProto p = Pass.Proto $ reifyKnownTypeT @Abstracted (prepareProto' p) . (head . view subDescs) {- we take type args here, cause we need only `t` instead of `Elem t` -} where
    prepareProto' :: forall p t m. (Logging m, KnownType (Abstract t), Pass.PassConstruction m, KnownElemPass p) => Template (Pass (ElemScope p t) m) -> Proxy t -> Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m))))
    prepareProto' = const . Event.Tagged (Event.fromPath @New) . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate

newtype ElemListenerPass p e = ElemListenerPass (forall t m. (KnownType (Abstract t), MonadPassManager m) => Listener p e (Elem t) (SubPass (ElemScope p t) m))
runElemListenerPass :: ElemListenerPass p e -> forall t m. (KnownType (Abstract t), MonadPassManager m) => Listener p e (Elem t) (SubPass (ElemScope p t) m)
runElemListenerPass (ElemListenerPass p) = p



tpElemPass :: forall p t e m a. a ~ Listener2 e (Elem t) (SubPass (ElemScope p t) m) => a -> a
tpElemPass = id ; {-# INLINE tpElemPass #-}

-------------------
-- === Model === --
-------------------

modelInit :: Req m '[Writer // Layer // Abstract (Elem t) // Model] => Listener2 New (Elem t) m
modelInit = listener2 . uncurry . flip $ writeLayer @Model ; {-# INLINE modelInit #-}
makeLayerGen2 'modelInit
modelInitPass = tpElemPass @ModelInit modelInit

modelImport :: Listener2 Import (Elem t) m
modelImport = listener2 $ undefined
makeLayerGen2 'modelImport
modelImportPass = tpElemPass @ModelImport modelImport


-- FIXME[WD -> MK]: it should not be defined for SomeExprLink, cause it just brokes type assumptions. It should be for (forall l. Expr l)
watchLinkImport :: Listener2 Import (ExprLink a b) m
watchLinkImport = listener2 $ \(t, exprTrans, _) -> undefined -- modifyLayer_ @Model (over both exprTrans) t
makeLayerGen2 'watchLinkImport
watchLinkImportPass = tpElemPass @WatchLinkImport watchLinkImport


-- FIXME[WD -> MK]: it should not be defined for SomeExpr, cause it just brokes type assumptions. It should be for (forall l. Expr l)
watchExprImport :: Listener2 Import (Expr l) m
watchExprImport = listener2 $ \(t, _, linkTrans) -> undefined -- inplaceModifyFieldsWith linkTrans t
makeLayerGen2 'watchExprImport
watchExprImportPass = tpElemPass @WatchExprImport watchExprImport

init1 :: MonadPassManager m => m ()
init1 = do
    addGenericElemEventListener @Model modelInitPass
    addGenericElemEventListener @Model modelImportPass
    addSpecificElemEventListener @Model watchLinkImportPass
    addSpecificElemEventListener @Model watchExprImportPass



-----------------
-- === UID === --
-----------------

nextUID :: PrimMonad m => STRefM m ID -> m ID
nextUID ref = Store.modifySTRef' ref $ id &&& succ ; {-# INLINE nextUID #-}

initUID :: Req m '[Writer // Layer // Abstract (Elem t) // UID] => STRefM m ID -> Listener2 New (Elem t) m
initUID ref = listener2 $ \(t, _) -> flip (writeLayer @UID) t =<< nextUID ref ; {-# INLINE initUID #-}
makeLayerGen2 'initUID
initUIDPass = tpElemPass @InitUID . initUID

watchUIDImport :: Req m '[Writer // Layer // Abstract (Elem t) // UID]
               => STRefM m ID -> Listener2 Import (Elem t) m
watchUIDImport ref = listener2 $ \(t, _, _) -> flip (writeLayer @UID) t =<< nextUID ref ; {-# INLINE watchUIDImport #-}
makeLayerGen2 'watchUIDImport
watchUIDImportPass = tpElemPass @WatchUIDImport . watchUIDImport

init2 :: MonadPassManager m => m ()
init2 = do
    ref <- Store.newSTRef (def :: ID)
    addGenericElemEventListener @UID (initUIDPass        ref)
    addGenericElemEventListener @UID (watchUIDImportPass ref)



-------------------
-- === Succs === --
-------------------

initSuccs :: Req m '[Writer // Layer // Abstract (Elem t) // Succs] => Listener2 New (Elem t) m
initSuccs = listener2 $ \(t, _) -> writeLayer @Succs mempty t ; {-# INLINE initSuccs #-}
makeLayerGen2 'initSuccs
initSuccsPass = tpElemPass @InitSuccs initSuccs


watchSuccs :: Req m '[Editor // Layer // AnyExpr // Succs] => Listener2 New (ExprLink a b) m
watchSuccs = listener2 $ \(t, (src, tgt)) -> modifyLayer_ @Succs (Set.insert $ unsafeGeneralize t) src ; {-# INLINE watchSuccs #-}
makeLayerGen2 'watchSuccs
watchSuccsPass = tpElemPass @WatchSuccs watchSuccs

-- FIXME[WD -> MK]: it should not be defined for SomeExpr, cause it just brokes type assumptions. It should be for (forall l. Expr l)
watchSuccsImport :: Listener2 Import (Expr l) m
watchSuccsImport = listener2 $ \(t, _, linkTrans) -> undefined -- modifyLayer_ @Succs (Set.map linkTrans) t ; {-# INLINE watchSuccsImport #-}
makeLayerGen2 'watchSuccsImport
watchSuccsImportPass = tpElemPass @WatchSuccsImport watchSuccsImport

watchRemoveEdge :: Req m '[ Reader // Layer // AnyExprLink // Model
                          , Editor // Layer // AnyExpr // Succs]
                => Listener2 Delete (ExprLink a b) m
watchRemoveEdge = listener2 $ \t -> do
    (src, tgt) <- readLayer @Model t
    modifyLayer_ @Succs (Set.delete $ unsafeGeneralize t) src
{-# INLINE watchRemoveEdge #-}
makeLayerGen2 'watchRemoveEdge
watchRemoveEdgePass = tpElemPass @WatchRemoveEdge watchRemoveEdge

watchRemoveNode :: Req m '[ Reader  // Layer  // AnyExpr // '[Model, Type]
                          , Editor  // Net    // AnyExprLink
                          , Emitter // Delete // AnyExprLink
                          ]
                 => Listener2 Delete (Expr l) m
watchRemoveNode = listener2 $ \t -> do
    inps   <- symbolFields (generalize t :: SomeExpr)
    tp     <- readLayer @Type t
    delete tp
    mapM_ delete inps
{-# INLINE watchRemoveNode #-}
makeLayerGen2 'watchRemoveNode
watchRemoveNodePass = tpElemPass @WatchRemoveNode watchRemoveNode

init3 :: MonadPassManager m => m ()
init3 = do
    addGenericElemEventListener  @Succs initSuccsPass
    addSpecificElemEventListener @Succs watchSuccsPass
    addSpecificElemEventListener @Succs watchSuccsImportPass
    addSpecificElemEventListener @Succs watchRemoveEdgePass
    addSpecificElemEventListener @Succs watchRemoveNodePass



------------------
-- === Type === --
------------------


consTypeLayer :: Req m '[ Writer  // Net // GraphElems
                        , Emitter // New // GraphElems]
              => Store.STRefM m (Maybe (Expr Star)) -> Expr t -> m (LayerData Type (Expr t))
consTypeLayer ref self = (`link` self) =<< unsafeRelayout <$> localTop ref ; {-# INLINE consTypeLayer #-}


localTop :: Req m '[Writer // Net // AnyExpr, Emitter // New // AnyExpr]
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

initType :: Req m '[ Writer  // Layer // AnyExpr // Type
                   , Writer  // Net   // '[AnyExpr, AnyExprLink]
                   , Emitter // New   // '[AnyExpr, AnyExprLink]
                   ]
         => STRefM m (Maybe (Expr Star)) -> Listener2 New (Expr l) m
initType ref = listener2 $ \(el, _) -> flip (writeLayer @Type) el =<< consTypeLayer ref el
{-# INLINE initType #-}
makeLayerGen2 'initType
initTypePass = tpElemPass @InitType . initType

-- FIXME[WD -> MK]: it should not be defined for SomeExpr, cause it just brokes type assumptions. It should be for (forall l. Expr l)
watchTypeImport :: Listener2 Import (Expr l) m
watchTypeImport = listener2 $ \(t, _, linkTrans) -> undefined -- modifyLayer_ @Type linkTrans t ; {-# INLINE watchTypeImport #-}
makeLayerGen2 'watchTypeImport
watchTypeImportPass = tpElemPass @WatchTypeImport watchTypeImport

init4 :: MonadPassManager m => m ()
init4 = do
    ref <- Store.newSTRef (Nothing :: Maybe (Expr Star))
    addSpecificElemEventListener @Type (initTypePass ref)
    addSpecificElemEventListener @Type watchTypeImportPass





-------------------------------------------
-------------------------------------------
-------------------------------------------
-------------------------------------------

runRegs :: MonadPassManager m => m ()
runRegs = do
    runElemRegs

    init1
    init2
    init3
    init4

    attachLayer 0 (getTypeDesc @Model) (getTypeDesc @AnyExpr)
    attachLayer 0 (getTypeDesc @Model) (getTypeDesc @AnyExprLink)
    attachLayer 5 (getTypeDesc @UID)   (getTypeDesc @AnyExpr)
    attachLayer 5 (getTypeDesc @UID)   (getTypeDesc @AnyExprLink)
    attachLayer 5 (getTypeDesc @Succs) (getTypeDesc @AnyExpr)

    attachLayer 10 (getTypeDesc @Type) (getTypeDesc @AnyExpr)





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

-- layerRegs :: MonadIR m => [m ()]
-- layerRegs = [] -- [layerReg1, layerReg2, layerReg3, layerReg4]
--
-- runLayerRegs :: MonadIR m => m ()
-- runLayerRegs = sequence_ layerRegs





----------------------------------
----------------------------------
----------------------------------


source :: (MonadRef m, Reader Layer (Abstract (Link a b) // Model) m) => Link a b -> m a
source = fmap fst . readLayer @Model ; {-# INLINE source #-}

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
