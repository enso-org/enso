{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.IR.ToRefactor2 where

import Luna.Prelude hiding (String, log, nested)
import qualified Luna.Prelude as Prelude
import Control.Arrow ((&&&))
import Data.STRef    (STRef)

import OCI.IR.Class
-- import qualified Luna.IR.Expr as Term
import qualified Data.ManagedVectorMap as Store
import OCI.IR.Layout.Class
import OCI.IR.Layout.Typed
import OCI.IR.Layer.Class
import Luna.IR.Layer.Type
import OCI.IR.Layer.Model
import Luna.IR.Layer.Succs
import OCI.IR.Layer.Req
import Luna.IR.Format
import Data.Property
import qualified OCI.Pass        as Pass
import           OCI.Pass        (Pass, Preserves, Inputs, Outputs, Events, SubPass, Uninitialized, Template, ElemScope, KnownElemPass, elemPassDescription, genericDescription, genericDescriptionP)
import Data.TypeDesc
import Data.Event (type (//), Tag(Tag), Emitter, PayloadData, Event, Emitters)
import qualified Data.Set as Set
import Data.ManagedVectorMap (STRef, STRefM, modifySTRef', newSTRef, readSTRef, writeSTRef)
-- import Luna.IR.Expr
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
import OCI.IR.Term

type GraphElems = '[AnyExpr, AnyExprLink]


instance {-# OVERLAPPABLE #-} TypePretty (ElemScope p t) where formatType [p,_] = [p]



data Abstracted a
type instance Abstract (TypeRef s) = TypeRef (Abstracted s)



-------------------------------------------

newtype Listener event m = Listener (PayloadData event -> m ())
makeLenses ''Listener



listener :: (PayloadData event -> m ()) -> Listener event m
listener = wrap'

runListener :: Listener event m -> (PayloadData event -> m ())
runListener = unwrap'
                -- => Template (SubPass pass m a) -> Uninitialized m (Template (m a))

makeOrdinaryEventListener :: forall p event m. (Pass.PassInit p (GetRefHandler m), Pass.KnownPass p)
    => Listener event (SubPass p (GetRefHandler m)) -> Pass.Describbed (Uninitialized (GetRefHandler m) (Template (GetRefHandler m ())))
makeOrdinaryEventListener p = Pass.describbed @p $ Pass.compileTemplate $ Pass.template $ runListener p

makeOrdinaryEventListener2 :: forall tag p event m. (Event.KnownTag tag, MonadPassManager m, Pass.PassInit p (GetRefHandler m), Pass.KnownPass p)
    => Listener event (SubPass p (GetRefHandler m)) -> m ()
makeOrdinaryEventListener2 p = addEventListener @tag $ Pass.describbed @p $ Pass.compileTemplate $ Pass.template $ runListener p


addElemEventListener :: forall l p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e)
                       => (forall t. KnownType (Abstract t) => Listener (e // Elem t) (SubPass (ElemScope p t) (GetRefHandler m))) -> m ()
addElemEventListener p = registerGenericElemEventListener (getTypeDesc @l) (getTypeDesc @p) $ prepareProto @e $ Pass.template $ runListener p

unsafeAddSpecificElemEventListener :: forall l t p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e, KnownType (Abstract t))
                       => Listener (e // Elem t) (SubPass (ElemScope p t) (GetRefHandler m)) -> m ()
unsafeAddSpecificElemEventListener p = registerSpecificElemEventListener (getTypeDesc @(Abstract (Elem t))) (getTypeDesc @l) (getTypeDesc @p) $ compileListener p


addExprEventListener :: forall l p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e)
                     => (forall t. Listener (e // Expr t) (SubPass (ElemScope p (EXPR t)) (GetRefHandler m))) -> m ()
addExprEventListener = unsafeAddSpecificElemEventListener @l

addExprLinkEventListener :: forall l p e m. (MonadPassManager m, KnownElemPass p, KnownType l, KnownType p, Event.KnownTag e)
                         => (forall a b. Listener (e // ExprLink a b) (SubPass (ElemScope p (LINK (Expr a) (Expr b))) (GetRefHandler m))) -> m ()
addExprLinkEventListener = unsafeAddSpecificElemEventListener @l




prepareProto :: forall e p m. (Logging m, Pass.PassConstruction m, KnownElemPass p, Event.KnownTag e) => (forall s. TypeReify (Abstracted s) => Template (Pass (ElemScope p (TypeRef s)) m)) -> Pass.Proto (Event.Tagged (Pass.Describbed (Uninitialized m (Template (m ())))))
prepareProto p = Pass.Proto $ reifyKnownTypeT @Abstracted (prepareProto' @e p) . (head . view subDescs) {- we take type args here, cause we need only `t` instead of `Elem t` -} where
    prepareProto' :: forall e p t m. (Logging m, KnownType (Abstract t), Pass.PassConstruction m, KnownElemPass p, Event.KnownTag e) => Template (Pass (ElemScope p t) m) -> Proxy t -> Event.Tagged (Pass.Describbed (Uninitialized m (Template (m ()))))
    prepareProto' = const . Event.Tagged (Event.fromPath @e) . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate


compileListener :: forall p e t m. (KnownType (Abstract t), KnownElemPass p, Pass.PassInit (ElemScope p (Elem t)) m, Event.KnownTag e)
                => Listener (e // Elem t) (SubPass (ElemScope p t) m) -> Event.Tagged (Pass.Describbed (Uninitialized m (Template (m ()))))
compileListener = Event.Tagged (Event.fromPath @e) . Pass.describbed @(ElemScope p (Elem t)) . Pass.compileTemplate . Pass.template . runListener




tpElemPass :: forall p t e m a. a ~ Listener (e // Elem t) (SubPass (ElemScope p t) m) => Proxy p -> a -> a
tpElemPass _ = id






----------------------------------
----------------------------------
----------------------------------

{-# DEPRECATED source "Use readSource instead" #-}
source :: (MonadRef m, Reader Layer (Abstract (Link a b) // Model) m) => Link a b -> m a
source = fmap fst . getLayer @Model

type SourceReadCtx s t m = (MonadRef m, Reader Layer (Abstract (Link s t) // Model) m)
type SourceEditCtx s t m = (MonadRef m, Editor Layer (Abstract (Link s t) // Model) m)

readSource  :: SourceReadCtx s t m => Link s t      -> m s
writeSource :: SourceEditCtx s t m => Link s t -> s -> m ()
readSource      = fst .: getLayer @Model
writeSource l s = modifyLayer_ @Model l $ _1 .~ s

readSources :: (SourceReadCtx s t m, Traversable f) => f (Link s t) -> m (f s)
readSources = mapM readSource

modifySourceM  :: forall s t m a. SourceEditCtx s t m => Link s t -> (s -> m (a, s)) -> m a
modifySourceM_ :: forall s t m.   SourceEditCtx s t m => Link s t -> (s -> m     s)  -> m ()
modifySource   :: forall s t m a. SourceEditCtx s t m => Link s t -> (s ->   (a, s)) -> m a
modifySource_  :: forall s t m.   SourceEditCtx s t m => Link s t -> (s ->       s)  -> m ()
modifySource   t   = modifySourceM  @s t . fmap return
modifySource_  t   = modifySourceM_ @s t . fmap return
modifySourceM_ t   = modifySourceM  @s t . (fmap.fmap) ((),)
modifySourceM  t f = do (a,s) <- f =<< readSource @s t
                        a <$ writeSource @s t s


readInputSources a = readSources =<< inputs a



type TargetReadCtx s t m = (MonadRef m, Reader Layer (Abstract (Link s t) // Model) m)
type TargetEditCtx s t m = (MonadRef m, Editor Layer (Abstract (Link s t) // Model) m)

readTarget  :: TargetReadCtx s t m => Link s t      -> m t
writeTarget :: TargetEditCtx s t m => Link s t -> t -> m ()
readTarget      = snd .: getLayer @Model
writeTarget l s = modifyLayer_ @Model l $ _2 .~ s

modifyTargetM  :: forall s t m a. TargetEditCtx s t m => Link s t -> (t -> m (a, t)) -> m a
modifyTargetM_ :: forall s t m.   TargetEditCtx s t m => Link s t -> (t -> m     t)  -> m ()
modifyTarget   :: forall s t m a. TargetEditCtx s t m => Link s t -> (t ->   (a, t)) -> m a
modifyTarget_  :: forall s t m.   TargetEditCtx s t m => Link s t -> (t ->       t)  -> m ()
modifyTarget   t   = modifyTargetM  @s t . fmap return
modifyTarget_  t   = modifyTargetM_ @s t . fmap return
modifyTargetM_ t   = modifyTargetM  @s t . (fmap.fmap) ((),)
modifyTargetM  t f = do (a,s) <- f =<< readTarget @s t
                        a <$ writeTarget @s t s


-- === KnownExpr === --

type KnownExpr l m = (MonadRef m, Readers Layer '[AnyExpr // Model, Link' AnyExpr // Model] m)

{-# DEPRECATED readTerm "Use `read` instead" #-}
readTerm :: forall l m. KnownExpr l m => Expr l -> m (ExprHeadDef l)
readTerm = unsafeToExprTermDef @(ExprHead l)

{-# DEPRECATED getSource "Use `readSourceAt` instead" #-}
getSource :: KnownExpr l m => Lens' (ExprHeadDef l) (ExprLink a b) -> Expr l -> m (Expr a)
getSource f v = readTerm v >>= readSource . view f

-- FIXME[WD]: We should generalize `read` to support reading both Exprs' as well as any other graph elem (links, groups, etc)
read :: forall l m. KnownExpr l m => Expr l -> m (Unwrapped (ExprHeadDef l))
read = fmap unwrap . unsafeToExprTermDef @(ExprHead l)

readAt :: forall l m a. KnownExpr l m => Lens' (Unwrapped (ExprHeadDef l)) a -> Expr l -> m a
readAt f t = view f <$> read t

-- FIXME[WD]: We could generalize `readSourceAt` to support not only Exprs
readSourceAt  :: (KnownExpr l m)                => Lens' (Unwrapped (ExprHeadDef l))    (ExprLink a b)  -> Expr l -> m    (Expr a)
readSourcesAt :: (KnownExpr l m, Traversable f) => Lens' (Unwrapped (ExprHeadDef l)) (f (ExprLink a b)) -> Expr l -> m (f (Expr a))
readSourceAt  f v = readAt f v >>=      readSource
readSourcesAt f v = readAt f v >>= mapM readSource


infixl 8 @.
(@.)  :: KnownExpr l m => Expr l -> Lens' (Unwrapped (ExprHeadDef l)) a -> m a
v @. f = readAt f v

infixl 8 @^.
infixl 8 @@^.
(@^.)  :: (KnownExpr l m)                => Expr l -> Lens' (Unwrapped (ExprHeadDef l))    (ExprLink a b)  -> m    (Expr a)
(@@^.) :: (KnownExpr l m, Traversable f) => Expr l -> Lens' (Unwrapped (ExprHeadDef l)) (f (ExprLink a b)) -> m (f (Expr a))
v @^.  f = readSourceAt  f v
v @@^. f = readSourcesAt f v


readWrappedSource :: (Unwrapped term ~ ExprLink a b, Wrapped term, term ~ Unwrapped (ExprHeadDef l), KnownExpr l m) => Expr l -> m (Expr a)
readWrappedSource = readSourceAt wrapped'

readWrappedSources :: (Unwrapped term ~ f (ExprLink a b), Wrapped term, Traversable f, term ~ Unwrapped (ExprHeadDef l), KnownExpr l m) => Expr l -> m (f (Expr a))
readWrappedSources = readSourcesAt wrapped'

modifyExprTerm :: forall l m. (KnownExpr l m, Writer Layer (AnyExpr // Model) m) => Expr l -> (ExprHeadDef l -> ExprHeadDef l) -> m ()
modifyExprTerm = unsafeModifyExprTermDef @(ExprHead l)


type family Head a

type instance Access AnyExpr (ET e _) = e
type instance Access AnyExpr (E  e  ) = e

type instance Access AnyExpr (TermicType a) = TermicType a

type instance Head (TermicType a) = TermicType a

type ExprHead l = Head (l # AnyExpr)
type ExprHeadDef l = ExprTermDef (ExprHead l) (Expr l)


-- FIXME[WD]: This should be deleted ASAP.
instance MonadLogging m => MonadLogging (DepState.StateT a b m)
