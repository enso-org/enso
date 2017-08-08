module Luna.Syntax.Text.Lexer.Analysis where

import Prologue                      hiding (span)
import Data.Text.Position            (Delta)
import Luna.Syntax.Text.Lexer.Symbol
import Luna.Syntax.Text.Lexer.Token


-----------------------------------
-- === Simple analysis utils === --
-----------------------------------

tagDisabled :: HasSymbol a => [Token a] -> [(Bool, Token a)]
tagDisabled = columnToDisabledTag . tagColumn mempty ; {-# INLINE tagDisabled #-}

tagColumn :: HasSymbol a => Delta -> [Token a] -> [(Delta, Token a)]
tagColumn d = \case
    []     -> []
    (a:as) -> (d,a) : tagColumn d' as where
        d' = case a ^. symbol of
            EOL -> a ^. offset
            s   -> d + a ^. span + a ^. offset
{-# NOINLINE tagColumn #-}

columnToDisabledTag  :: HasSymbol a =>                [(Delta, Token a)] -> [(Bool, Token a)]
columnToDisabledTag' :: HasSymbol a => Maybe Delta -> [(Delta, Token a)] -> [(Bool, Token a)]
columnToDisabledTag      = columnToDisabledTag' Nothing ; {-# INLINE columnToDisabledTag #-}
columnToDisabledTag' ind = \case
    []        -> []
    ((d,t):s) -> (checkCurrent d, t) : columnToDisabledTag' ind' s where
        ind' = case t ^. symbol of
            Disable -> disableCurrent d
            _       -> updateCurrent d

    where checkCurrent   :: Delta -> Bool
          disableCurrent :: Delta -> Maybe Delta
          updateCurrent  :: Delta -> Maybe Delta
          checkCurrent   d = maybe False (d>) ind
          disableCurrent d = Just $ maybe d (min d) ind
          updateCurrent  d = ind >>= \d' -> if d > d' then Just d' else Nothing
{-# INLINE columnToDisabledTag' #-}
