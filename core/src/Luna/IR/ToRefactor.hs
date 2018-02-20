{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.IR.ToRefactor (module Luna.IR.ToRefactor, module X) where

import Luna.Prelude hiding (String, log, nested)
import qualified Luna.Prelude as Prelude
import Control.Arrow ((&&&))
import Data.STRef    (STRef)

import OCI.IR.Class
import qualified Luna.IR.Expr as Term
import qualified Data.ManagedVectorMap as Store
import OCI.IR.Layout.Class
import OCI.IR.Layout.Typed
import OCI.IR.Layer.Class
import Luna.IR.Layer.Type
import OCI.IR.Layer.Model
import Luna.IR.Layer.Succs
import OCI.IR.Layer.Req
import Luna.IR.Format
import Luna.IR.Layer.UID        as UID
import Luna.IR.Layer.Errors     as Errors
import Luna.IR.Layer.RequiredBy as RequiredBy
import Luna.IR.Layer.UserType   as UserType
import Luna.IR.Layer.Requester  as Requester
import Data.Property
import qualified OCI.Pass        as Pass
import           OCI.Pass        (Pass, Preserves, Inputs, Outputs, Events, SubPass, Uninitialized, Template, ElemScope, KnownElemPass, elemPassDescription, genericDescription, genericDescriptionP)
import Data.TypeDesc
import Data.Event (type (//), Tag(Tag), Emitter, PayloadData, Event, Emitters)
import qualified Data.Set as Set
import Data.ManagedVectorMap (STRef, STRefM, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Luna.IR.Expr hiding (Import)
import Unsafe.Coerce (unsafeCoerce)
import OCI.Pass.Manager as PM
import qualified Data.Event as Event
import System.Log
import qualified Control.Monad.State.Dependent.Old as DepState

import qualified GHC.Exts as Prim

import Data.Reflection (Reifies)
import OCI.Pass.Definition

import Type.Any (AnyType)
import Control.Monad.Raise
import OCI.IR.Name

import Luna.IR.ToRefactor2 as X
import Control.Monad.Trans.Except (ExceptT)

generalize' :: AssertGeneralizable a SomeExpr => a -> SomeExpr
generalize' = generalize


-------------------
-- === Model === --
-------------------

modelInit :: Req m '[Writer // Layer // Abstract (Elem t) // Model] => Listener (New // Elem t) m
modelInit = listener . uncurry $ putLayer @Model
makePass 'modelInit

-- FIXME[WD -> MK]: this implementation is not understandable, please refactor the names, make local variables etc. (For example name "exprTranslator" does not tell anything to the reader)
watchLinkImport :: Req m '[Editor // Layer // AnyExprLink // Model] => Listener (Import // ExprLink a b) m
watchLinkImport = listener $ \(t, trans, _) -> modifyLayer_ @Model t $ (_1 %~ trans ^. exprTranslator) . (_2 %~ trans ^. exprTranslator)
makePass 'watchLinkImport

-- FIXME[WD -> MK]: this implementation is not understandable, please refactor the names, make local variables etc. (For example name "linkTranslator" does not tell anything to the reader)
watchExprImport :: Req m '[Editor // Layer // AnyExpr // Model] => Listener (Import // Expr t) m
watchExprImport = listener $ \(t, trans, _) -> inplaceModifyFieldsWith (trans ^. linkTranslator) (generalize' t)
makePass 'watchExprImport


getInputsOnQueryChildren :: Req m '[ Reader // Layer // AnyExpr // Model ] => Listener (OnQueryChildren // Expr l) m
getInputsOnQueryChildren = listener $ \(t, ref) -> do
    inps <- inputs t
    Store.modifyUnsafeSTRef ref (fmap generalize inps ++)
makePass 'getInputsOnQueryChildren

-- | We say that a given node A is safe to remove (i.e. not an input to any other node) when all the successors have the form A -> A.
--   Warning: this assumes that we have only cycles of length 1, any longer cycle will always be unsafe to remove for now.
isSafeToRemove :: forall t m. (MonadRef m, Readers Net '[AnyExprLink, AnyExpr] m, Readers Layer '[AnyExpr // Succs, AnyExprLink // Model] m)
               => Expr t -> m Bool
isSafeToRemove expr = do
    succs <- getLayer @Succs expr
    and . fmap (uncurry (==)) <$> mapM (fmap (_1 %~ generalize) . getLayer @Model) (Set.toList succs)

removeInputsOnDeepDeleteNode :: Req m '[ Reader  // Layer // AnyExprLink // Model
                                       , Reader  // Layer // AnyExpr // Succs
                                       , Editor  // Net   // AnyExpr
                                       , Editor  // Net   // AnyExprLink
                                       , Emitter // OnDeepDelete    // AnyExpr
                                       , Emitter // Delete          // AnyExpr
                                       , Emitter // OnQueryChildren // AnyExpr
                                       ] => Listener (OnDeepDelete // Expr l) m
removeInputsOnDeepDeleteNode = listener $ \(t, s) -> when (Set.notMember (generalize t) s) $ whenM (isSafeToRemove t) $ do
    inps <- queryChildren t
    srcs <- mapM readSource inps
    delete t
    mapM_ (flip deepDeleteWithWhitelist s) $ Set.toList $ Set.delete (generalize t) $ Set.fromList srcs
makePass 'removeInputsOnDeepDeleteNode

init1 :: MonadPassManager m => m ()
init1 = do
    addElemEventListener     @Model modelInitPass
    addExprEventListener     @Model watchExprImportPass
    addExprEventListener     @Model removeInputsOnDeepDeleteNodePass
    addExprEventListener     @Model getInputsOnQueryChildrenPass
    addExprLinkEventListener @Model watchLinkImportPass







-------------------
-- === Succs === --
-------------------

initSuccs :: Req m '[Writer // Layer // Abstract (Elem t) // Succs] => Listener (New //Elem t) m
initSuccs = listener $ \(t, _) -> putLayer @Succs t mempty
makePass 'initSuccs

registerSuccOnNewLink :: Req m '[Editor // Layer // AnyExpr // Succs] => Listener (New // ExprLink a b) m
registerSuccOnNewLink = listener $ \(t, (src, tgt)) -> modifyLayer_ @Succs src $ Set.insert $ unsafeGeneralize t
makePass 'registerSuccOnNewLink

registerSuccOnExprImport :: Req m '[Editor // Layer // AnyExpr // Succs] => Listener (Import // Expr l) m
registerSuccOnExprImport = listener $ \(t, trans, _) -> modifyLayer_ @Succs t $ Set.map $ trans ^. linkTranslator
makePass 'registerSuccOnExprImport

removeSuccOnRemoveLink :: Req m '[ Reader // Layer // AnyExprLink // Model
                                 , Editor // Layer // AnyExpr     // Succs
                                 ] => Listener (Delete // ExprLink a b) m
removeSuccOnRemoveLink = listener $ \t -> do
    (src, tgt) <- getLayer @Model t
    modifyLayer_ @Succs src $ Set.delete $ unsafeGeneralize t
makePass 'removeSuccOnRemoveLink

removeInputsOnRemoveNode :: Req m '[ Reader  // Layer  // AnyExpr // Model
                                   , Editor  // Net    // AnyExprLink
                                   , Emitter // Delete // AnyExprLink
                                   ] => Listener (Delete // Expr l) m
removeInputsOnRemoveNode = listener $ mapM_ delete <=< inputs . generalize'
makePass 'removeInputsOnRemoveNode


init3 :: MonadPassManager m => m ()
init3 = do
    addElemEventListener     @Succs initSuccsPass
    addExprEventListener     @Succs registerSuccOnExprImportPass
    addExprEventListener     @Succs removeInputsOnRemoveNodePass
    addExprLinkEventListener @Model registerSuccOnNewLinkPass  -- FIXME[WD]: We need to express better pass deps here. We attach pass to Link's Model, because there is no way to attach pass to
                                                               -- Link based on layers attached to nodes (like - if node has Succs, attach this to Link)
    addExprLinkEventListener @Model removeSuccOnRemoveLinkPass



------------------
-- === Type === --
------------------

consTypeLayer :: Req m '[ Writer  // Net // GraphElems
                        , Emitter // New // GraphElems]
              => STRefM m (Maybe (Expr Star)) -> Expr t -> m (LayerData Type (Expr t))
consTypeLayer ref self = (`link` self) =<< unsafeRelayout <$> localTop ref

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

initType :: Req m '[ Writer  // Layer // AnyExpr // Type
                   , Writer  // Net   // '[AnyExpr, AnyExprLink]
                   , Emitter // New   // '[AnyExpr, AnyExprLink]
                   ]
         => STRefM m (Maybe (Expr Star)) -> Listener (New // Expr l) m
initType ref = listener $ \(el, _) -> putLayer @Type el =<< consTypeLayer ref el
makePass 'initType

watchTypeImport :: Req m '[Editor // Layer // AnyExpr // Type] => Listener (Import // Expr l) m
watchTypeImport = listener $ \(t, trans, _) -> modifyLayer_ @Type t $ trans ^. linkTranslator
makePass 'watchTypeImport

removeTypeOnRemoveNode :: Req m '[ Reader  // Layer  // AnyExpr // Type
                                 , Editor  // Net    // AnyExprLink
                                 , Emitter // Delete // AnyExprLink
                                 ]
                       => Listener (Delete // Expr l) m
removeTypeOnRemoveNode = listener $ getLayer @Type >=> delete
makePass 'removeTypeOnRemoveNode

getTypeOnQueryChildren :: Req m '[ Reader // Layer // AnyExpr // Type ] => Listener (OnQueryChildren // Expr l) m
getTypeOnQueryChildren = listener $ \(t, ref) -> do
    tp <- getLayer @Type t
    Store.modifyUnsafeSTRef ref (generalize tp :)
makePass 'getTypeOnQueryChildren


init4 :: MonadPassManager m => m ()
init4 = do
    ref <- newSTRef (Nothing :: Maybe (Expr Star))
    addExprEventListener @Type   $ initTypePass ref
    addExprEventListener @Type   $ watchTypeImportPass
    addExprEventListener @Type   $ removeTypeOnRemoveNodePass
    addExprEventListener @Type   $ getTypeOnQueryChildrenPass


-------------------------------------------

runRegs :: (MonadPassManager m, Throws IRError m) => m ()
runRegs = runRegs' True

runRegs' :: (MonadPassManager m, Throws IRError m) => Bool -> m ()
runRegs' withType = do
    runElemRegs

    init1
    UID.init
    Requester.init
    Errors.init
    RequiredBy.init
    UserType.init
    init3
    when withType init4

    attachLayer 0 (getTypeDesc @Model)      (getTypeDesc @AnyExpr)
    attachLayer 0 (getTypeDesc @Model)      (getTypeDesc @AnyExprLink)
    attachLayer 5 (getTypeDesc @UID)        (getTypeDesc @AnyExpr)
    attachLayer 5 (getTypeDesc @UID)        (getTypeDesc @AnyExprLink)
    attachLayer 5 (getTypeDesc @Errors)     (getTypeDesc @AnyExpr)
    attachLayer 5 (getTypeDesc @RequiredBy) (getTypeDesc @AnyExpr)
    attachLayer 5 (getTypeDesc @Succs)      (getTypeDesc @AnyExpr)
    attachLayer 5 (getTypeDesc @UserType)   (getTypeDesc @AnyExpr)

    when withType $ do
        attachLayer 10 (getTypeDesc @Type)   (getTypeDesc @AnyExpr)

    attachLayer 15 (getTypeDesc @Requester) (getTypeDesc @AnyExpr)





-- === Elem reg defs === --

runElemRegs :: MonadIR m => m ()
runElemRegs = sequence_ $ elemReg1 : elemReg2 : elemReg3 : []

elemReg1 :: MonadIR m => m ()
elemReg1 = registerElem @AnyExpr

elemReg2 :: MonadIR m => m ()
elemReg2 = registerElem @(Link' AnyExpr)

elemReg3 :: MonadIR m => m ()
elemReg3 = registerElem @(GROUP AnyExpr)
