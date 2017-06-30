module Luna.Syntax.Text.Parser.AST where

import Prologue
import Luna.IR
import qualified Data.Set as Set


-- === Utils === --

getParent :: Req m '[ Reader // Layer // AnyExprLink // Model
                    , Reader // Layer // AnyExpr     // Succs
                    ]
          => SomeExpr -> m (Maybe SomeExpr)
getParent a = (Set.elems <$> getLayer @Succs a) >>= \case
    []  -> return Nothing
    [p] -> Just <$> readTarget p
    _   -> error "Impossible happened"
