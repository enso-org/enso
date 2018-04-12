module Luna.Build.Dependency.Resolver ( solveConstraints ) where

import Prologue hiding ((.>))

import qualified Prelude as P

import qualified Data.Text as Text

import qualified Control.Monad.Trans as C
import qualified Data.Map.Strict     as M
import qualified Data.Traversable    as T

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe

import Data.SBV

import Luna.Build.Dependency.Constraint

import Debug.Trace

-- TODO [Ara] define this ourselves
solverConfig :: SMTConfig
solverConfig = defaultSMTCfg -- current default is Z3, so leave it as is

data SVersion = SVersion
    { __major             :: SInteger
    , __minor             :: SInteger
    , __patch             :: SInteger
    , __prerelease        :: SInteger
    , __prereleaseVersion :: SInteger
    } deriving (Eq, Generic, Show)
makeLenses ''SVersion

instance Mergeable SVersion

instance EqSymbolic SVersion where
    SVersion a1 b1 c1 d1 e1 .== SVersion a2 b2 c2 d2 e2 =
        (a1, b1, c1, d1, e1) .== (a2, b2, c2, d2, e2)

instance OrdSymbolic SVersion where
    SVersion a1 b1 c1 d1 e1 .< SVersion a2 b2 c2 d2 e2 =
        (a1, b1, c1, d1, e1) .< (a2, b2, c2, d2, e2)

instance (Provable p) => Provable (SVersion -> p) where
    forAll_ f    = forAll_ (\(a,b,c,d,e)    -> f (SVersion a b c d e))
    forAll ns f  = forAll ns (\(a,b,c,d,e)  -> f (SVersion a b c d e))
    forSome_ f   = forSome_ (\(a,b,c,d,e)   -> f (SVersion a b c d e))
    forSome ns f = forSome ns (\(a,b,c,d,e) -> f (SVersion a b c d e))

sVersion :: String -> Symbolic SVersion
sVersion name = do
    a <- free $ name <> "-major"
    b <- free $ name <> "-minor"
    c <- free $ name <> "-patch"
    d <- free $ name <> "-prerelease"
    e <- free $ name <> "-prereleaseVersion"
    pure $ SVersion a b c d e

literalSVersion :: Integer -> Integer -> Integer -> Integer -> Integer
                -> Symbolic SVersion
literalSVersion a b c d e =
    pure $ SVersion (literal a) (literal b) (literal c) (literal d) (literal e)

constraintPredicate :: ConstraintMap -> Predicate
constraintPredicate constraints = do
    freeV1 <- sVersion "foo"
    literalV1 <- literalSVersion 0 0 1 3 0
    fooMin <- literalSVersion 0 0 1 3 0

    pure $ freeV1 .< literalV1 &&& freeV1 .>= fooMin

-- TODO [Ara] Can we construct a metric function from the result to maximise?
-- TODO [Ara] Needs to take list of available versions. (foo == a ||| b ||| c)
-- TODO [Ara] Turn result into resolved deps
solveConstraints :: (MonadIO m) => ConstraintMap -> m (Maybe Int)
solveConstraints constraints = do
    r@(SatResult modelResult) <- liftIO $ runSolver constraints
    traceShowM r
    case modelResult of
        Unsatisfiable _ -> pure $ Just 0
        Satisfiable _ model -> do
            traceShowM model
            pure $ Just 1
        SatExtField _ model -> pure $ Just 1
        Unknown _ reason -> traceShowM reason >> pure $ Just 0
        ProofError _ xs -> traceShowM xs >> pure $ Just 0

runSolver :: ConstraintMap -> IO SatResult
runSolver constraints = sat $ constraintPredicate constraints

