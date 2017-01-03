{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE UndecidableInstances      #-}

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
import           Luna.Pass        (Pass, Preserves, Inputs, Outputs, Events, SubPass, Uninitialized, Template, DynPass, ElemScope, KnownElemPass, elemPassDescription, genericDescription, genericDescriptionP)
import Data.TypeDesc
import Data.Event (type (//), Tag(Tag), Emitter, PayloadData, Event, Emitters)
import qualified Data.Set as Set
import Luna.IR.Internal.LayerStore (STRef, STRefM)
import Luna.IR.Expr
import Unsafe.Coerce (unsafeCoerce)
import Luna.Pass.Manager as PM
-- import Data.Event as Event
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


newtype ListenerPassDef p e t m = ListenerPassDef (PayloadData (e // t) -> PassDef p m)
makeWrapped ''ListenerPassDef

type SpecInitializer p t m = ListenerPassDef (Init p) New t m
type GenInitializer  p t m = SpecInitializer p (Elem t) m

listenerPassDef :: (PayloadData (e // t) -> m ()) -> ListenerPassDef p e t m
listenerPassDef = wrap' . fmap subPassDef ; {-# INLINE listenerPassDef #-}

runListenerPassDef :: ListenerPassDef p e t m -> (PayloadData (e // t) -> m ())
runListenerPassDef = fmap runSubPassdef . unwrap' ; {-# INLINE runListenerPassDef #-}




compileListener :: forall p e t m. (KnownType (Abstract t), KnownElemPass p, Pass.PassInit (ElemScope p (Elem t)) m)
                => ListenerPassDef p e (Elem t) (SubPass (ElemScope p t) m) -> Pass.Describbed (Uninitialized m (Template (DynPass m)))
compileListener = Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate . Pass.template . runListenerPassDef




registerSpecLayer :: forall p e t m. (MonadPassManager m, KnownElemPass (Init p), KnownType p, KnownType (Abstract t))
                  => ListenerPassDef (Init p) e (Elem t) (SubPass (ElemScope (Init p) t) (GetRefHandler m)) -> m ()
registerSpecLayer p = registerLayerProto (getTypeDesc @p) $ Pass.constProto $ compileListener p ; {-# INLINE registerSpecLayer #-}



prepareProto :: forall p m. (Logging m, Pass.PassConstruction m, KnownElemPass p) => (forall s. TypeReify (Abstracted s) => Template (Pass (ElemScope p (TypeRef s)) m)) -> Pass.Proto (Pass.Describbed (Uninitialized m (Template (DynPass m))))
prepareProto p = Pass.Proto $ reifyKnownTypeT @Abstracted (prepareProto' p) . (head . view subDescs) {- we take type args here, cause we need only `t` instead of `Elem t` -} where
    prepareProto' :: forall p t m. (Logging m, KnownType (Abstract t), Pass.PassConstruction m, KnownElemPass p) => Template (Pass (ElemScope p t) m) -> Proxy t -> Pass.Describbed (Uninitialized m (Template (DynPass m)))
    prepareProto' = const . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate


registerGenLayer :: forall p e m. (MonadPassManager m, KnownElemPass (Init p), KnownType p)
                  => (forall t. KnownType (Abstract t) => ListenerPassDef (Init p) e (Elem t) (SubPass (ElemScope (Init p) t) (GetRefHandler m)))
                  -> m ()
registerGenLayer p = registerLayerProto (getTypeDesc @p) $ prepareProto $ Pass.template $ runListenerPassDef p




-------------------
-- === Model === --
-------------------

initModel :: Req m '[Writer // Layer // Abstract (Elem t) // Model] => GenInitializer Model t m
initModel = listenerPassDef . uncurry . flip $ writeLayer @Model ; {-# INLINE initModel #-}
makeLayerGen ''Model 'initModel

init1 :: MonadPassManager m => m ()
init1 = registerGenLayer @Model initModel



-----------------
-- === UID === --
-----------------

initUID :: Req m '[Writer // Layer // Abstract (Elem t) // UID] => STRefM m ID -> GenInitializer UID t m
initUID ref = listenerPassDef $ \(t, tdef) -> do
    nuid <- Store.modifySTRef' ref (\i -> (i, succ i))
    writeLayer @UID nuid t
{-# INLINE initUID #-}
makeLayerGen ''UID 'initUID

init2 :: MonadPassManager m => m ()
init2 = do
    ref <- Store.newSTRef (def :: ID)
    registerGenLayer @UID (initUID ref)



-------------------
-- === Succs === --
-------------------

initSuccs :: Req m '[Writer // Layer // Abstract (Elem t) // Succs] => GenInitializer Succs t m
initSuccs = listenerPassDef $ \(t, _) -> writeLayer @Succs mempty t ; {-# INLINE initSuccs #-}
makeLayerGen ''Succs 'initSuccs

data WatchSuccs
watchSuccs :: Req m '[Editor // Layer // AnyExpr // Succs] => ListenerPassDef WatchSuccs New (ExprLink' l) m
watchSuccs = listenerPassDef $ \(t, (src, tgt)) -> modifyLayer_ @Succs (Set.insert $ unsafeGeneralize t) src ; {-# INLINE watchSuccs #-}
makePass 'watchSuccs

data WatchRemoveEdge
watchRemoveEdge :: Req m '[ Reader // Layer // AnyExprLink // Model
                          , Editor // Layer // AnyExpr // Succs]
                => ListenerPassDef WatchRemoveEdge Delete (ExprLink' l) m
watchRemoveEdge = listenerPassDef $ \t -> do
    (src, tgt) <- readLayer @Model t
    modifyLayer_ @Succs (Set.delete $ unsafeGeneralize t) src
{-# INLINE watchRemoveEdge #-}
makePass 'watchRemoveEdge

data WatchRemoveNode
watchRemoveNode :: Req m '[ Reader  // Layer  // AnyExpr // '[Model, Type]
                          , Editor  // Net    // AnyExprLink
                          , Emitter // Delete // AnyExprLink
                          ]
                 => ListenerPassDef WatchRemoveNode Delete (Expr l) m
watchRemoveNode = listenerPassDef $ \t -> do
    inps   <- symbolFields (generalize t :: SomeExpr)
    tp     <- readLayer @Type t
    delete tp
    mapM_ delete inps
{-# INLINE watchRemoveNode #-}
makePass 'watchRemoveNode

init3 :: MonadPassManager m => m ()
init3 = do
    registerGenLayer @Succs initSuccs
    addEventListener @(New    // AnyExprLink) $ compileListener watchSuccs
    addEventListener @(Delete // AnyExprLink) $ compileListener watchRemoveEdge
    addEventListener @(Delete // AnyExpr)     $ compileListener watchRemoveNode



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
         => STRefM m (Maybe (Expr Star)) -> SpecInitializer Type (Expr l) m
initType ref = listenerPassDef $ \(el, _) -> flip (writeLayer @Type) el =<< consTypeLayer ref el
{-# INLINE initType #-}
makeLayerGen ''Type 'initType

init4 :: MonadPassManager m => m ()
init4 = do
    ref <- Store.newSTRef (Nothing :: Maybe (Expr Star))
    registerSpecLayer @Type $ initType ref





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



    -- addEventListener 100 (Tag [getTypeDesc @Delete, getTypeDesc @(Link' AnyExpr)]) $ foo watchRemoveNode


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
