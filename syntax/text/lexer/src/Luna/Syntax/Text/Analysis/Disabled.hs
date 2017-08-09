{-# LANGUAGE Strict #-}

module Luna.Syntax.Text.Analysis.Disabled where

import Prologue                      hiding (span)
import Data.Text.Position            (Delta)
import Luna.Syntax.Text.Lexer.Symbol
import Luna.Syntax.Text.Lexer.Token


-----------------------------------
-- === Simple analysis utils === --
-----------------------------------

type ColumnStack = [Delta]

tagDisabled  :: HasSymbol a =>                [Token a] -> [(ColumnStack, Token a)]
tagDisabled' :: HasSymbol a => ColumnStack -> [Token a] -> [(ColumnStack, Token a)]
tagDisabled    = tagDisabled' mempty                       ; {-# INLINE tagDisabled  #-}
tagDisabled' s = columnToDisabledTag' s . tagColumn mempty ; {-# INLINE tagDisabled' #-}

tagColumn :: HasSymbol a => Delta -> [Token a] -> [(Delta, Token a)]
tagColumn d = \case
    []     -> []
    (a:as) -> (d,a) : tagColumn d' as where
        d' = case a ^. symbol of
            EOL -> a ^. offset
            s   -> d + a ^. span + a ^. offset
{-# NOINLINE tagColumn #-}

columnToDisabledTag  :: HasSymbol a =>                [(Delta, Token a)] -> [(ColumnStack, Token a)]
columnToDisabledTag' :: HasSymbol a => ColumnStack -> [(Delta, Token a)] -> [(ColumnStack, Token a)]
columnToDisabledTag  = columnToDisabledTag' mempty ; {-# INLINE columnToDisabledTag #-}
columnToDisabledTag' disabledStack = \case
    []        -> []
    ((d,t):s) -> (curDisabledStack, t) : columnToDisabledTag' subDisabledStack s where
        curDisabledStack = updateCurrent d disabledStack
        subDisabledStack = case t ^. symbol of
            Disable -> disableCurrent d
            _       -> curDisabledStack

    where checkCurrent   :: Delta -> Bool
          disableCurrent :: Delta -> ColumnStack
          updateCurrent  :: Delta -> ColumnStack -> ColumnStack
          checkCurrent   d = maybe False (d>) (maybeHead disabledStack)
          disableCurrent d = d : disabledStack
          updateCurrent  d s = go s where
              go = \case []     -> []
                         (i:is) -> if d > i then s else go is
{-# INLINE columnToDisabledTag' #-}
