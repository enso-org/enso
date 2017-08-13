{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType           #-}

module OCI.IR.Layer.Req where

import Prologue_old hiding (pprint, Type)

import OCI.IR.Class
import Data.Event
import qualified Data.Constraint.Struct as Constraint


------------------------------------
-- === Construction utilities === --
------------------------------------

-- | Following type families allow for pass description generation out of
--   function constraint. They could be used as follow:
--
--       data InitModel
--       type instance Abstract InitModel = InitModel
--       type instance Inputs  Net   (ElemScope InitModel t) = GetInputs  Net   (InitModelCtx t AnyType)
--       type instance Outputs Net   (ElemScope InitModel t) = GetOutputs Net   (InitModelCtx t AnyType)
--       ...
--       type instance Outputs Event (ElemScope InitModel t) = GetEmitters      (InitModelCtx t AnyType)
--       type instance Inputs  Event (ElemScope InitModel t) = '[]
--       type instance Preserves     (ElemScope InitModel t) = '[]
--       instance KnownElemPass InitModel where
--           elemPassDescription = genericDescriptionP
--
--   where `InitModelCtx` is just a function constraint providing pass requirements (writers / readers / emitters / etc.)

type family FilterInputs t (a :: Constraint) :: [*] where
    FilterInputs t (a,as)         = FilterInputs t a <> FilterInputs t as
    FilterInputs t (Reader t a m) = '[a]
    FilterInputs t (Editor t a m) = '[a]
    FilterInputs t a              = '[]

type family FilterOutputs t (a :: Constraint) :: [*] where
    FilterOutputs t (a,as)         = FilterOutputs t a <> FilterOutputs t as
    FilterOutputs t (Writer t a m) = '[a]
    FilterOutputs t (Editor t a m) = '[a]
    FilterOutputs t a              = '[]

type family FilterEmitters (a :: Constraint) :: [*] where
    FilterEmitters (a,as)        = FilterEmitters a <> FilterEmitters as
    FilterEmitters (Emitter e m) = '[e]
    FilterEmitters a             = '[]

type GetInputs   t c = FilterInputs   t (Constraint.Recursive c)
type GetOutputs  t c = FilterOutputs  t (Constraint.Recursive c)
type GetEmitters e   = FilterEmitters   (Constraint.Recursive e)



-----------------
-- === Req === --
-----------------

-- | Req is a pass requirements description sugar. It allows defining complex pass dependencies using simple, unified syntax.
--   It should be straightforward to understand and use, after seeing some examples:
--
--       Req m '[Reader // Layer // Model // '[AnyExpr, AnyExprLink]]
--       <=>
--       (Reader Layer (Model // AnyExpr) m, Reader Layer (Model // AnyExprLink) m)
--
--       Req m '[Editor // Layer // '[Model, UID] // '[AnyExpr, AnyExprLink]]
--       <=>
--       (Editor Layer (Model // AnyExpr) m, Editor Layer (UID // AnyExpr) m, Editor Layer (Model // AnyExprLink) m, Editor Layer (UID // AnyExprLink) m)
--
--       etc.

type Req m (rs :: [*]) = (ConstraintReq m (ExpandAll (Proxy rs)), MonadRef m)

type family ConstraintReq m ls :: Constraint where
    ConstraintReq m '[] = ()
    ConstraintReq m ((Reader  // t // s) ': rs) = (Reader  t s m, ConstraintReq m rs)
    ConstraintReq m ((Writer  // t // s) ': rs) = (Writer  t s m, ConstraintReq m rs)
    ConstraintReq m ((Editor  // t // s) ': rs) = (Editor  t s m, ConstraintReq m rs)
    ConstraintReq m ((Emitter // e)      ': rs) = (Emitter e   m, ConstraintReq m rs)

type family Prep a (ls :: [*]) :: [*] where
    Prep (Proxy a) '[]       = '[]
    Prep (Proxy a) (l ': ls) = (a // l) ': Prep (Proxy a) ls

type family Expand ls :: [*] where
    Expand (Proxy (l // ls))   = Prep (Proxy l) (Expand (Proxy ls))
    Expand (Proxy (ls :: [k])) = ExpandAll (Proxy ls)
    Expand (Proxy (ls ::  *))  = '[ls]

type family ExpandAll (ls :: *) :: [*] where
    ExpandAll (Proxy '[])       = '[]
    ExpandAll (Proxy (l ': ls)) = Expand (Proxy l) <> ExpandAll (Proxy ls)
