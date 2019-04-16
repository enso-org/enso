module Luna.Prim.Package where

import Prologue

import qualified Data.Map                    as Map
import qualified Luna.IR                     as IR
import qualified Luna.Package.Environment    as PackageEnv
import qualified Luna.Pass.Sourcing.Data.Def as Def
import qualified Luna.Runtime                as Runtime
import qualified Luna.Std.Builder            as Builder

import Data.Map (Map)

exports
    :: forall graph m. (Builder.StdBuilder graph m) => m (Map IR.Name Def.Def)
exports = do
    primPackageNameToEnvVarName <- Builder.makeFunction @graph
        (flip Runtime.toValue PackageEnv.packageNameToEnvVarName)
        [Builder.textLT]
        Builder.textLT
    pure $ Map.fromList
        [("primPackageNameToEnvVarName", primPackageNameToEnvVarName)]
