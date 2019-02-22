module Prologue.Data.Foldable (module Prologue.Data.Foldable, module X) where

import Prelude                   hiding (minimum, maximum)
import Prologue.Data.Maybe       (fromJust)
import Prologue.Data.Traversable

import Control.Monad
import           Data.Kind       (Constraint, Type)
import qualified Data.Foldable      as F
import qualified Data.Bifoldable    as F
import qualified Control.Error.Safe as S
import           Data.Foldable      as X (Foldable  , fold  , foldMap  , foldr  , foldr'  , foldl  , foldl'  , elem  , sum  , product  , foldrM  , foldlM)
import           Data.Bifoldable    as X (Bifoldable, bifold, bifoldMap, bifoldr, bifoldr', bifoldl, bifoldl', bielem, bisum, biproduct, bifoldrM, bifoldlM)
import           Data.Foldable      as X (traverse_  , for_  , asum)
import           Data.Bifoldable    as X (bitraverse_, bifor_, biasum)
import           Data.Foldable      as X (concat  , concatMap  , and  , or  , any  , all)
import           Data.Bifoldable    as X (biconcat, biconcatMap, biand, bior, biany, biall)

import Data.Semigroup.Foldable as X (Foldable1, fold1, foldMap1, toNonEmpty)


type family Foldables (lst :: [Type -> Type]) :: Constraint where
    Foldables '[]       = ()
    Foldables (t ': ts) = (Foldable t, Foldables ts)

sequence_   :: (Foldable   t, Applicative f) => t (f a)       -> f ()
bisequence_ :: (Bifoldable t, Applicative f) => t (f a) (f b) -> f ()
sequence_   = F.sequenceA_   ; {-# INLINE sequence_   #-}
bisequence_ = F.bisequenceA_ ; {-# INLINE bisequence_ #-}

traverse2_ :: (Applicative m, Foldables '[t1, t2])             => (a -> m b) -> t1 (t2 a)                -> m ()
traverse3_ :: (Applicative m, Foldables '[t1, t2, t3])         => (a -> m b) -> t1 (t2 (t3 a))           -> m ()
traverse4_ :: (Applicative m, Foldables '[t1, t2, t3, t4])     => (a -> m b) -> t1 (t2 (t3 (t4 a)))      -> m ()
traverse5_ :: (Applicative m, Foldables '[t1, t2, t3, t4, t5]) => (a -> m b) -> t1 (t2 (t3 (t4 (t5 a)))) -> m ()
traverse2_ = traverse_ . traverse_  ; {-# INLINE traverse2_ #-}
traverse3_ = traverse_ . traverse2_ ; {-# INLINE traverse3_ #-}
traverse4_ = traverse_ . traverse3_ ; {-# INLINE traverse4_ #-}
traverse5_ = traverse_ . traverse4_ ; {-# INLINE traverse5_ #-}

unsafeMinimum :: Ord a => [a] -> a
unsafeMaximum :: Ord a => [a] -> a
unsafeMinimum = F.minimum ; {-# INLINE unsafeMinimum #-}
unsafeMaximum = F.maximum ; {-# INLINE unsafeMaximum #-}

minimum :: MonadPlus m => Ord a => [a] -> m a
maximum :: MonadPlus m => Ord a => [a] -> m a
minimum = S.minimumZ ; {-# INLINE minimum #-}
maximum = S.maximumZ ; {-# INLINE maximum #-}

minimumDef :: Ord a => a -> [a] -> a
maximumDef :: Ord a => a -> [a] -> a
minimumDef d = fromJust d . minimum ; {-# INLINE minimumDef #-}
maximumDef d = fromJust d . maximum ; {-# INLINE maximumDef #-}

-- === Deprecations === --

mapM_   :: (Foldable   t, Applicative f) => (a -> f b)               -> t a   -> f ()
bimapM_ :: (Bifoldable t, Applicative f) => (a -> f c) -> (b -> f d) -> t a b -> f ()
mapM_   = traverse_   ; {-# INLINE mapM_   #-}
bimapM_ = bitraverse_ ; {-# INLINE bimapM_ #-}

{-# DEPRECATED forM_ "Use `for_` instead" #-}
forM_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
forM_ = for_ ; {-# INLINE forM_ #-}
