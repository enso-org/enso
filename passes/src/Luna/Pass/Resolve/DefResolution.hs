{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Resolve.DefResolution where

import Prologue

import qualified Data.Graph.Data.Layer.Layout        as Layout
import qualified Luna.IR                             as IR
import qualified Luna.IR.Layer                       as Layer
import qualified Luna.Pass                           as Pass
import qualified Luna.Pass.Attr                      as Attr
import qualified Luna.Pass.Basic                     as Pass

import Luna.Pass.Data.Root
import Luna.Pass.Resolve.Data.UnresolvedVariables
import Luna.Pass.Resolve.Data.Resolution

data DefResolution

type instance Pass.Spec DefResolution t = DefResolutionSpec t
type family DefResolutionSpec t where
    DefResolutionSpec (Pass.In  Pass.Attrs) = '[UnresolvedVariables, DefResolver]
    DefResolutionSpec (Pass.Out Pass.Attrs) = '[UnresolvedVariables]
    DefResolutionSpec t = Pass.BasicPassSpec t

instance ( Pass.Interface DefResolution (Pass.Pass stage DefResolution)
         , IR.DeleteSubtree (Pass.Pass stage DefResolution)
         ) => Pass.Definition stage DefResolution where
    definition = do
        UnresolvedVariables vars <- Attr.get
        Attr.put $ UnresolvedVariables []
        traverse_ resolveDef vars

resolveDef :: ( Pass.Interface DefResolution m
              , IR.DeleteSubtree m
              ) => IR.Term IR.Var -> m ()
resolveDef v = do
    IR.Var n   <- IR.model v
    resolver   <- Attr.get @DefResolver
    let resolution = resolve n resolver
    {-putStrLn $ show n <> " : " <> show resolution-}
    case resolution of
        Resolved (DefRef m) -> do
            resolved <- IR.resolvedDef m n
            IR.replace resolved v
        _ -> return ()
