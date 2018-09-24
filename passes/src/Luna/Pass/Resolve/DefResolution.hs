{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Resolve.DefResolution where

import Prologue

import qualified Luna.IR              as IR
import qualified Luna.Pass            as Pass
import qualified Luna.Pass.Attr       as Attr
import qualified Luna.Pass.Data.Error as Error
import qualified Luna.Pass.Data.Stage as TC

import Luna.Pass.Resolve.Data.Resolution
import Luna.Pass.Resolve.Data.UnresolvedVariables

data DefResolution

type instance Pass.Spec DefResolution t = DefResolutionSpec t
type family DefResolutionSpec t where
    DefResolutionSpec (Pass.In  Pass.Attrs) = '[UnresolvedVariables, DefResolver]
    DefResolutionSpec (Pass.Out Pass.Attrs) = '[UnresolvedVariables]
    DefResolutionSpec t = Pass.BasicPassSpec t

instance Pass.Definition TC.Stage DefResolution where
    definition = do
        UnresolvedVariables vars <- Attr.get
        Attr.put $ UnresolvedVariables []
        traverse_ resolveDef vars

resolveDef :: IR.Term IR.Var -> TC.Pass DefResolution ()
resolveDef v = do
    IR.Var n   <- IR.modelView v
    resolver   <- Attr.get @DefResolver
    let resolution = resolve n resolver
    case resolution of
        Resolved (DefRef m) -> do
            resolved <- IR.resolvedDef m n
            IR.replace resolved v
        _ -> do
            Error.setError (Just $ Error.varNotFound n) v

