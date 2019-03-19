{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Sourcing.Utils where

import Prologue

import qualified Data.Text                             as Text
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Layer                         as Layer



-----------------
-- === API === --
-----------------

cutDoc :: ( Layer.Reader IR.Term IR.Model m
          , Layer.Reader IR.Link IR.Source m )
    => IR.SomeTerm -> m (Maybe Text, IR.SomeTerm)
cutDoc t = Layer.read @IR.Model t >>= \case
    Uni.Documented doc term -> do
        (_, t') <- cutDoc =<< IR.source term
        d <- Text.pack <$> SmallVector.toList doc
        pure (Just d, t')
    Uni.Marked _ term -> cutDoc =<< IR.source term
    _ -> pure (Nothing, t)
{-# INLINE cutDoc #-}

