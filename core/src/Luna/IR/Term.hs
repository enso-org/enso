{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term (module Luna.IR.Term, module X) where

import Prologue

import Luna.IR.Term.Ast     as X
import Luna.IR.Term.Core    as X
import Luna.IR.Term.Format  as X (Ast, Draft, Literal, Phrase, Thunk, Value)
import Luna.IR.Term.Literal as X

import qualified Data.Generics.Traversable         as GTraversable
import qualified Luna.IR.Component.Link            as Link
import qualified Luna.IR.Component.Term.Class      as Term
import qualified Luna.IR.Component.Term.Definition as Term
import qualified OCI.IR.Layer                      as Layer

import qualified Data.Construction         as Data
import           Data.Generics.Traversable (GTraversable)


----------------------
-- === Uni Term === --
----------------------

-- | UniTerm is the collection of all possible IR terms.
--   The terms are discovered automatically and the UniTerm is generated below.
--   For more information, please refer to the TH funciton documentation.

-- === Definition === -

Term.makeUniTerm
type instance Term.Uni = UniTerm


-- === Instances === --

instance Link.Provider1 UniTerm where
    linksIO1 = Link.glinks ; {-# INLINE linksIO1 #-}

instance StyledShow Term.TagOnly (UniTerm a) where
    styledShow _ = GTraversable.gfoldl' @Term.ShowTag f mempty where
        f acc a = acc <> Term.showTag a

instance MonadIO m => Data.Destructor1 m UniTerm where
    destruct1 = GTraversable.gmapM_ @(GTraversable Layer.Destructor)
              $ GTraversable.gmapM_ @Layer.Destructor Layer.destruct
    {-# INLINE destruct1 #-}
