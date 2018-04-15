module Luna.Build.Dependency.Resolver
    ( module Luna.Build.Dependency.Resolver
    , module X
    ) where

import Prologue hiding (Constraint, Constraints)

import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import qualified Luna.Build.Dependency.Constraint as Constraint

import Luna.Build.Dependency.Resolver.Internal as X ( constraintQuery
                                                    , SolverError
                                                      ( UnavailablePackages
                                                      , PackagesNotProvided
                                                      , UnsatisfiableConstraints
                                                      , UnknownSolution
                                                      , BadOptimisation
                                                      , ExtensionField
                                                      , InternalError
                                                      , MissingVariables )
                                                    , solverConfig )



------------------------
-- === Solver API === --
------------------------

solveConstraints :: MonadIO m => Constraint.Constraints -> Constraint.Versions
                 -> m (Either SolverError Constraint.PackageSet)
solveConstraints constraints versions = do
    let constraintPackages = List.sort $ Map.keys constraints
        versionPackages    = List.sort $ Map.keys versions
        relSize            = List.length constraintPackages `compare`
                             List.length versionPackages

    case relSize of
        EQ -> liftIO (constraintQuery constraints versions)
        GT -> do
            let missingPackages =
                    filter (`notElem` versionPackages) constraintPackages
            pure . Left $ UnavailablePackages missingPackages
        LT -> do
            let missingPackages =
                    filter (`notElem` constraintPackages) versionPackages
            pure . Left $ PackagesNotProvided missingPackages

