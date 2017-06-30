module Luna.Syntax.Text.Parser.Reserved where

import Luna.Prelude hiding ((|>), SomeSymbol, Symbol)

import OCI.IR hiding (Expr, Layer, IRBuilder, Atom, modify_, get, put)
import Control.Monad.State.Dependent
import qualified Data.Set as Set
import           Data.Set (Set)
import Luna.Syntax.Text.Parser.Class (Symbol)


-------------------------
-- === Reservation === --
-------------------------

-- === Definition === --

data Reservation = Reservation { _global :: !(Set Symbol)
                               , _local  :: !(Set Symbol) -- reserved only for a particular depth
                               } deriving (Show)
makeLenses ''Reservation


-- === Management === --

withReservedSymbol  ::  MonadState Reservation m              =>   Symbol -> m a -> m a
withReservedSymbols :: (MonadState Reservation m, Foldable f) => f Symbol -> m a -> m a
withReservedSymbol  n  = withModified @Reservation (global %~ Set.insert n)
withReservedSymbols    = flip $ foldl (flip withReservedSymbol)

withLocalReservedSymbol   ::  MonadState Reservation m              =>   Symbol -> m a -> m a
withLocalUnreservedSymbol ::  MonadState Reservation m              =>   Symbol -> m a -> m a
withLocalReservedSymbols  :: (MonadState Reservation m, Foldable f) => f Symbol -> m a -> m a
withLocalReservedSymbol   n  = withModified @Reservation (local %~ Set.insert n)
withLocalUnreservedSymbol n  = withModified @Reservation (local %~ Set.delete n)
withLocalReservedSymbols     = flip $ foldl (flip withLocalReservedSymbol)

withNewLocal :: MonadState Reservation m => m a -> m a
withNewLocal = withModified @Reservation $ local .~ mempty

checkForSymbolReservation :: MonadState Reservation m => Symbol -> m Bool
checkForSymbolReservation n = flip lookupSymbolReservation n <$> get @Reservation

lookupSymbolReservation :: Reservation -> Symbol -> Bool
lookupSymbolReservation s n = (Set.member n $ view local s) || (Set.member n $ view global s)


-- === Instances === --

instance Default Reservation where def = Reservation def def
