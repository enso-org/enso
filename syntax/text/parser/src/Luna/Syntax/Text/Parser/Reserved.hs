module Luna.Syntax.Text.Parser.Reserved where

import Prologue hiding (SomeSymbol, Symbol, (|>))

import qualified Control.Monad.State.Layered as State
import qualified Data.Set                    as Set

import Data.Set                      (Set)
import Luna.Syntax.Text.Parser.Class (Symbol)
-- import           OCI.IR                        hiding (Atom, Expr, IRBuilder,
                                                -- Layer, get, modify_, put)


-------------------------
-- === Reservation === --
-------------------------

-- === Definition === --

data Reservation = Reservation { _global :: !(Set Symbol)
                               , _local  :: !(Set Symbol) -- reserved only for a particular depth
                               } deriving (Show)
makeLenses ''Reservation


-- === Management === --

withReservedSymbol  ::  State.Monad Reservation m              =>   Symbol -> m a -> m a
withReservedSymbols :: (State.Monad Reservation m, Foldable f) => f Symbol -> m a -> m a
withReservedSymbol  n  = State.withModified @Reservation (global %~ Set.insert n)
withReservedSymbols    = flip $ foldl (flip withReservedSymbol)

withLocalReservedSymbol   ::  State.Monad Reservation m              =>   Symbol -> m a -> m a
withLocalUnreservedSymbol ::  State.Monad Reservation m              =>   Symbol -> m a -> m a
withLocalReservedSymbols  :: (State.Monad Reservation m, Foldable f) => f Symbol -> m a -> m a
withLocalReservedSymbol   n  = State.withModified @Reservation (local %~ Set.insert n)
withLocalUnreservedSymbol n  = State.withModified @Reservation (local %~ Set.delete n)
withLocalReservedSymbols     = flip $ foldl (flip withLocalReservedSymbol)

withNewLocal :: State.Monad Reservation m => m a -> m a
withNewLocal = State.withModified @Reservation $ local .~ mempty

checkForSymbolReservation :: State.Monad Reservation m => Symbol -> m Bool
checkForSymbolReservation n = flip lookupSymbolReservation n <$> State.get @Reservation

lookupSymbolReservation :: Reservation -> Symbol -> Bool
lookupSymbolReservation s n = (Set.member n $ view local s) || (Set.member n $ view global s)


-- === Instances === --

instance Default Reservation where def = Reservation def def
