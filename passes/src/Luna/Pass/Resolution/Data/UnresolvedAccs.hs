module Luna.Pass.Resolution.Data.UnresolvedAccs where

import Luna.IR
import Luna.Prelude
import OCI.Pass.Manager
import Data.TypeDesc
import qualified Data.Set as Set
import           Data.Set (Set)

newtype UnresolvedAccs = UnresolvedAccs (Set (Expr Monadic))
makeWrapped ''UnresolvedAccs

addAcc :: Expr Monadic -> UnresolvedAccs -> UnresolvedAccs
addAcc el = wrap . Set.insert el . unwrap

putAccs :: [Expr Monadic] -> UnresolvedAccs
putAccs = wrap . Set.fromList

getAccs :: UnresolvedAccs -> [Expr Monadic]
getAccs = Set.toList . unwrap

initUnresolvedAccs :: MonadPassManager m => m ()
initUnresolvedAccs = setAttr (getTypeDesc @UnresolvedAccs) $ UnresolvedAccs Set.empty
