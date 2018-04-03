module Luna.IR.Term (module Luna.IR.Term, module X) where

import Prologue

import Luna.IR.Term.Core   as X
import Luna.IR.Term.Format as X (Ast, Draft, Literal, Phrase, Thunk, Value)

import qualified Luna.IR.Component.Term.Class      as Term
import qualified Luna.IR.Component.Term.Definition as Term
import qualified OCI.IR.Layer.Internal             as Layer


----------------------
-- === Uni Term === --
----------------------

-- | UniTerm is the collection of all possible IR terms.
--   The terms are discovered automatically and the UniTerm is generated below.
--   For more information, please look at the TH funciton documentation.

-- === Definition === -

Term.makeUniTerm
type instance Term.Uni = UniTerm


-- === Instances === --

instance Layer.DataInitializer UniTerm where
    initStaticData = Just $ UniTermMissing Missing ; {-# INLINE initStaticData #-}
