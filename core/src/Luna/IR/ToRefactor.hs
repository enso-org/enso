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
import Luna.IR.Internal.LayerStore (STRef, STRefM, modifySTRef', newSTRef, readSTRef, writeSTRef)
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

newtype Listener e t m = Listener (PayloadData (e // t) -> m ())
makeWrapped ''Listener



listener :: (PayloadData (e // t) -> m ()) -> Listener e t m
listener = wrap' ; {-# INLINE listener #-}

runListener :: Listener e t m -> (PayloadData (e // t) -> m ())
runListener = unwrap' ; {-# INLINE runListener #-}





addElemEventListener :: forall l p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e)
                       => (forall t. KnownType (Abstract t) => Listener e (Elem t) (SubPass (ElemScope p t) (GetRefHandler m))) -> m ()
addElemEventListener p = registerGenericElemEventListener (getTypeDesc @l) (getTypeDesc @p) $ prepareProto @e $ Pass.template $ runListener p

unsafeAddSpecificElemEventListener :: forall l t p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e, KnownType (Abstract t))
                       => Listener e (Elem t) (SubPass (ElemScope p t) (GetRefHandler m)) -> m ()
unsafeAddSpecificElemEventListener p = registerSpecificElemEventListener (getTypeDesc @(Abstract (Elem t))) (getTypeDesc @l) (getTypeDesc @p) $ compileListener p


addExprEventListener :: forall l p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e)
                     => (forall t. Listener e (Expr t) (SubPass (ElemScope p (EXPR t)) (GetRefHandler m))) -> m ()
addExprEventListener = unsafeAddSpecificElemEventListener @l ; {-# INLINE addExprEventListener #-}

addExprLinkEventListener :: forall l p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e)
                         => (forall a b. Listener e (ExprLink a b) (SubPass (ElemScope p (LINK (Expr a) (Expr b))) (GetRefHandler m))) -> m ()
addExprLinkEventListener = unsafeAddSpecificElemEventListener @l ; {-# INLINE addExprLinkEventListener #-}




prepareProto :: forall e p m. (Logging m, Pass.PassConstruction m, KnownElemPass p, Event.KnownTag e) => (forall s. TypeReify (Abstracted s) => Template (Pass (ElemScope p (TypeRef s)) m)) -> Pass.Proto (Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m)))))
prepareProto p = Pass.Proto $ reifyKnownTypeT @Abstracted (prepareProto' @e p) . (head . view subDescs) {- we take type args here, cause we need only `t` instead of `Elem t` -} where
    prepareProto' :: forall e p t m. (Logging m, KnownType (Abstract t), Pass.PassConstruction m, KnownElemPass p, Event.KnownTag e) => Template (Pass (ElemScope p t) m) -> Proxy t -> Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m))))
    prepareProto' = const . Event.Tagged (Event.fromPath @e) . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate


compileListener :: forall p e t m. (KnownType (Abstract t), KnownElemPass p, Pass.PassInit (ElemScope p (Elem t)) m, Event.KnownTag e)
                => Listener e (Elem t) (SubPass (ElemScope p t) m) -> Event.Tagged (Pass.Describbed (Uninitialized m (Template (DynPass m))))
compileListener = Event.Tagged (Event.fromPath @e) . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate . Pass.template . runListener




tpElemPass :: forall p t e m a. a ~ Listener e (Elem t) (SubPass (ElemScope p t) m) => Proxy p -> a -> a
tpElemPass _ = id ; {-# INLINE tpElemPass #-}





-------------------
-- === Model === --
-------------------

modelInit :: Req m '[Writer // Layer // Abstract (Elem t) // Model] => Listener New (Elem t) m
modelInit = listener . uncurry . flip $ writeLayer @Model ; {-# INLINE modelInit #-}
makePass 'modelInit

watchLinkImport :: Req m '[Editor // Layer // AnyExprLink // Model] => Listener Import (ExprLink a b) m
watchLinkImport = listener $ \(t, trans) -> modifyLayer_ @Model ((_1 %~ trans ^. exprTranslator) . (_2 %~ trans ^. exprTranslator)) t
{-# INLINE watchLinkImport #-}
makePass 'watchLinkImport

watchExprImport :: Req m '[Editor // Layer // AnyExpr // Model] => Listener Import (Expr t) m
watchExprImport = listener $ \(t, trans) -> inplaceModifyFieldsWith (trans ^. linkTranslator) (generalize t :: SomeExpr) ; {-# INLINE watchExprImport #-}
makePass 'watchExprImport

init1 :: MonadPassManager m => m ()
init1 = do
    addElemEventListener     @Model modelInitPass
    addExprEventListener     @Model watchExprImportPass
    addExprLinkEventListener @Model watchLinkImportPass



-----------------
-- === UID === --
-----------------

nextUID :: PrimMonad m => STRefM m ID -> m ID
nextUID ref = modifySTRef' ref $ id &&& succ ; {-# INLINE nextUID #-}

initUID :: Req m '[Writer // Layer // Abstract (Elem t) // UID] => STRefM m ID -> Listener New (Elem t) m
initUID ref = listener $ \(t, _) -> flip (writeLayer @UID) t =<< nextUID ref ; {-# INLINE initUID #-}
makePass 'initUID

watchUIDImport :: Req m '[Writer // Layer // Abstract (Elem t) // UID]
               => STRefM m ID -> Listener Import (Elem t) m
watchUIDImport ref = listener $ \(t, _) -> flip (writeLayer @UID) t =<< nextUID ref ; {-# INLINE watchUIDImport #-}
makePass 'watchUIDImport

init2 :: MonadPassManager m => m ()
init2 = do
    ref <- newSTRef (def :: ID)
    addElemEventListener @UID (initUIDPass        ref)
    addElemEventListener @UID (watchUIDImportPass ref)



-------------------
-- === Succs === --
-------------------

initSuccs :: Req m '[Writer // Layer // Abstract (Elem t) // Succs] => Listener New (Elem t) m
initSuccs = listener $ \(t, _) -> writeLayer @Succs mempty t ; {-# INLINE initSuccs #-}
makePass 'initSuccs

watchSuccs :: Req m '[Editor // Layer // AnyExpr // Succs] => Listener New (ExprLink a b) m
watchSuccs = listener $ \(t, (src, tgt)) -> modifyLayer_ @Succs (Set.insert $ unsafeGeneralize t) src ; {-# INLINE watchSuccs #-}
makePass 'watchSuccs

watchSuccsImport :: Req m '[Editor // Layer // AnyExpr // Succs] => Listener Import (Expr l) m
watchSuccsImport = listener $ \(t, trans) -> modifyLayer_ @Succs (Set.map $ trans ^. linkTranslator) t ; {-# INLINE watchSuccsImport #-}
makePass 'watchSuccsImport

watchRemoveEdge :: Req m '[ Reader // Layer // AnyExprLink // Model
                          , Editor // Layer // AnyExpr // Succs]
                => Listener Delete (ExprLink a b) m
watchRemoveEdge = listener $ \t -> do
    (src, tgt) <- readLayer @Model t
    modifyLayer_ @Succs (Set.delete $ unsafeGeneralize t) src
{-# INLINE watchRemoveEdge #-}
makePass 'watchRemoveEdge

watchRemoveNode :: Req m '[ Reader  // Layer  // AnyExpr // '[Model, Type]
                          , Editor  // Net    // AnyExprLink
                          , Emitter // Delete // AnyExprLink
                          ]
                 => Listener Delete (Expr l) m
watchRemoveNode = listener $ \t -> do
    inps   <- symbolFields (generalize t :: SomeExpr)
    tp     <- readLayer @Type t
    delete tp
    mapM_ delete inps
{-# INLINE watchRemoveNode #-}
makePass 'watchRemoveNode

init3 :: MonadPassManager m => m ()
init3 = do
    addElemEventListener     @Succs initSuccsPass
    addExprEventListener     @Succs watchSuccsImportPass
    addExprEventListener     @Succs watchRemoveNodePass
    addExprLinkEventListener @Model watchSuccsPass
    addExprLinkEventListener @Model watchRemoveEdgePass



------------------
-- === Type === --
------------------


consTypeLayer :: Req m '[ Writer  // Net // GraphElems
                        , Emitter // New // GraphElems]
              => STRefM m (Maybe (Expr Star)) -> Expr t -> m (LayerData Type (Expr t))
consTypeLayer ref self = (`link` self) =<< unsafeRelayout <$> localTop ref ; {-# INLINE consTypeLayer #-}


localTop :: Req m '[Writer // Net // AnyExpr, Emitter // New // AnyExpr]
         => STRefM m (Maybe (Expr Star)) -> m (Expr Star)
localTop ref = readSTRef ref >>= \case
    Just t  -> return t
    Nothing -> do
        s <- reserveStar
        writeSTRef ref $ Just s
        registerStar s
        writeSTRef ref Nothing
        return s
{-# INLINE localTop #-}

initType :: Req m '[ Writer  // Layer // AnyExpr // Type
                   , Writer  // Net   // '[AnyExpr, AnyExprLink]
                   , Emitter // New   // '[AnyExpr, AnyExprLink]
                   ]
         => STRefM m (Maybe (Expr Star)) -> Listener New (Expr l) m
initType ref = listener $ \(el, _) -> flip (writeLayer @Type) el =<< consTypeLayer ref el
{-# INLINE initType #-}
makePass 'initType

watchTypeImport :: Req m '[Editor // Layer // AnyExpr // Type] => Listener Import (Expr l) m
watchTypeImport = listener $ \(t, trans) -> modifyLayer_ @Type (trans ^. linkTranslator) t ; {-# INLINE watchTypeImport #-}
makePass 'watchTypeImport

init4 :: MonadPassManager m => m ()
init4 = do
    ref <- newSTRef (Nothing :: Maybe (Expr Star))
    addExprEventListener @Type (initTypePass ref)
    addExprEventListener @Type watchTypeImportPass





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
