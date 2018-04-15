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
                                                      , UnsatisfiableConstraints
                                                      , UnknownSolution )
                                                    , solverConfig )



------------------------
-- === Solver API === --
------------------------

solveConstraints :: MonadIO m => Constraint.Constraints -> Constraint.Versions
                 -> m (Either SolverError Constraint.PackageSet)
solveConstraints constraints versions =
    if List.sort (Map.keys constraints) /= List.sort (Map.keys versions)
    then do
        let missingPackages = filter (\x -> x `notElem` Map.keys versions)
                            $ Map.keys constraints
        pure . Left $ UnavailablePackages missingPackages
    else liftIO (constraintQuery constraints versions)

