module Luna.Parser.Location where

import Prelude.Luna

import Data.Int (Int64)

import qualified Luna.Syntax.Model.Text.Location as Location
import qualified Text.Trifecta.Delta             as Trifecta
import qualified Text.Trifecta.Combinators       as Trifecta

import Luna.Syntax.Model.Text.Location (MonadLocation)
import Text.Trifecta.Combinators       (DeltaParsing)



-- === Utils === --

deltaLine :: Trifecta.Delta -> Int64
deltaLine = \case
    Trifecta.Columns  _ _       -> 0
    Trifecta.Tab      _ _ _     -> 0
    Trifecta.Lines    l _ _ _   -> l
    Trifecta.Directed _ l _ _ _ -> l

current :: DeltaParsing m => m Location.Grid
current = do
    d <- Trifecta.position
    return $ convert d

located :: (DeltaParsing p, MonadLocation m) => p (m a) -> p (m a)
located pf = do
    loc  <- current
    out  <- pf
    loc' <- current
    return $ Location.with (const $ Just $ Location.Delta loc loc') out


-- === Instances === --

instance Convertible Trifecta.Delta Location.Grid where
    convert d = Location.Grid (deltaLine d) (Trifecta.column d)


