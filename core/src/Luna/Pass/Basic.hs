{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Basic where

import Prologue hiding (Type)

import qualified Luna.IR.Component.Link as Link
import qualified OCI.Pass.Cache         as Pass
import qualified OCI.Pass.Registry      as Registry

-- import Luna.IR.Link        as X (Link, Links, Source, Target)
-- import Luna.IR.Term        as X (Model, Term, Terms)
import OCI.Pass.Definition as X (Attrs, Definition, Elems, In, Out, Preserves,
                                 Spec)

import Luna.IR
import Luna.IR.Term.Core ()



data BasicPass
type instance Spec BasicPass t = BasicPassSpec t
type family   BasicPassSpec  t where
    BasicPassSpec (In Elems) = '[Terms, Links]
    BasicPassSpec (In Terms) = '[Model, Type, Users]
    BasicPassSpec (In Links) = '[Source, Target]
    BasicPassSpec (In Attrs) = '[]
    BasicPassSpec (Out a)    = BasicPassSpec (In a)
    BasicPassSpec t          = '[]

Pass.cache_phase1 ''BasicPass
Pass.cache_phase2 ''BasicPass


-- defaultRegistry :: Registry.Monad m => m ()
-- defaultRegistry = do
--     Registry.registerComponent @Terms
--     Registry.registerPrimLayer @Terms @Model
--     -- Registry.registerPrimLayer @Terms @Type

--     Registry.registerComponent @Links
--     Registry.registerPrimLayer @Links @Source
--     Registry.registerPrimLayer @Links @Target
