{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

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
            _   -> d + a ^. span + a ^. offset
{-# NOINLINE tagColumn #-}

columnToDisabledTag  :: forall a. HasSymbol a =>                [(Delta, Token a)] -> [(ColumnStack, Token a)]
columnToDisabledTag' :: forall a. HasSymbol a => ColumnStack -> [(Delta, Token a)] -> [(ColumnStack, Token a)]
columnToDisabledTag  = columnToDisabledTag' mempty ; {-# INLINE columnToDisabledTag #-}
columnToDisabledTag' disabledStack = \case
    []        -> []
    ((d,t):s) -> (curDisabledStack, t) : columnToDisabledTag' subDisabledStack s where
        curDisabledStack = updateCurrent t d disabledStack
        subDisabledStack = case t ^. symbol of
            Disable -> disableCurrent d
            _       -> curDisabledStack

    where disableCurrent :: Delta -> ColumnStack
          updateCurrent  :: Token a -> Delta -> ColumnStack -> ColumnStack
          disableCurrent d = d : disabledStack
          updateCurrent  t d s = case t ^. symbol of
              STX -> s
              _   -> go s where
                  go = \case []     -> []
                             (i:is) -> if d > i then (i:is) else go is
{-# INLINE columnToDisabledTag' #-}

