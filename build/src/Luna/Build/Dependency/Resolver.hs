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

data RVersion = RVersion
    { __major :: Integer
    , __minor :: Integer
    , __patch :: Integer
    , __prerelease :: Integer
    , __prereleaseVersion :: Integer
    } deriving (Eq, Ord, Show, Read, Data)

instance SymWord RVersion
instance HasKind RVersion
instance SatModel RVersion

type SVersion = SBV RVersion

mkSVersion :: Integer -> Integer -> Integer -> Integer -> Integer -> SVersion
mkSVersion a b c d e = literal $ RVersion a b c d e

sVersion :: String -> Symbolic SVersion
sVersion = free

sVersion_ :: Symbolic SVersion
sVersion_ = free_

data Foo = Foo SInt8 SInt8 deriving Generic

instance EqSymbolic Foo where
  Foo x1 y1 .== Foo x2 y2 = (x1, y1) .== (x2, y2)

instance Mergeable Foo

instance OrdSymbolic Foo where
  Foo x1 y1 .< Foo x2 y2 = (x1, y1) .< (x2, y2)

instance Provable p => Provable (Foo -> p) where
  forAll_ f = forAll_ (\(x,y) -> f (Foo x y))
  forAll ns f = forAll ns (\(x,y) -> f (Foo x y))
  forSome_ f = forSome_ (\(x,y) -> f (Foo x y))
  forSome ns f = forSome ns (\(x,y) -> f (Foo x y))

mkSFoo :: Int8 -> Int8 -> Symbolic Foo
mkSFoo i j = do
  i' <- pure (literal i)
  j' <- pure (literal j)
  pure $ Foo i' j'

constraintPredicate :: ConstraintMap -> Predicate
constraintPredicate constraints = do
    let keys = M.keys constraints
        elems = M.elems constraints

    staticV1 <- mkSFoo 1 2
    varV1 <- Foo <$> sInt8 "i1" <*> sInt8 "i2"

    pure $ varV1 .< staticV1

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

