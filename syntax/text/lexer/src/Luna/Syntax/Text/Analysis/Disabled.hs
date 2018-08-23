{-# LANGUAGE NoStrict #-}

module Luna.Syntax.Text.Analysis.Disabled where

import Prologue hiding (span)

import qualified Luna.Syntax.Text.Lexer.Symbol as Symbol
import qualified Luna.Syntax.Text.Lexer.Token  as Token

import Data.Text.Position           (Delta)
import Luna.Syntax.Text.Lexer.Token (Token)



-----------------------------------
-- === Simple analysis utils === --
-----------------------------------

type ColumnStack = [Delta]

tagDisabled  ::                [Token] -> [(ColumnStack, Token)]
tagDisabled' :: ColumnStack -> [Token] -> [(ColumnStack, Token)]
tagDisabled  = tagDisabled' mempty
tagDisabled' = \s -> colToDisabledTag' s . tagColumn mempty
{-# INLINE tagDisabled  #-}
{-# INLINE tagDisabled' #-}

tagColumn :: Delta -> [Token] -> [(Delta, Token)]
tagColumn = \d -> \case
    []     -> []
    (a:as) -> (d,a) : tagColumn d' as where
        d' = case a ^. Symbol.symbol of
            Symbol.EOL -> a ^. Token.offset
            _          -> d + a ^. Token.span + a ^. Token.offset
{-# NOINLINE tagColumn #-}

colToDisabledTag  ::                [(Delta, Token)] -> [(ColumnStack, Token)]
colToDisabledTag' :: ColumnStack -> [(Delta, Token)] -> [(ColumnStack, Token)]
colToDisabledTag  = colToDisabledTag' mempty
colToDisabledTag' disabledStack = \case
    []        -> []
    ((d,t):s) -> (curDisabledStack, t) : colToDisabledTag' subDisabledStack s
        where curDisabledStack = updateCurrent t d disabledStack
              subDisabledStack = case t ^. Symbol.symbol of
                  Symbol.Disable -> disableCurrent d
                  _              -> curDisabledStack

    where disableCurrent :: Delta -> ColumnStack
          updateCurrent  :: Token -> Delta -> ColumnStack -> ColumnStack
          disableCurrent d = d : disabledStack
          updateCurrent  t d s = case t ^. Symbol.symbol of
              Symbol.STX -> s
              _          -> go s where
                  go = \case []     -> []
                             (i:is) -> if d > i then (i:is) else go is
{-# INLINE colToDisabledTag #-}
{-# INLINE colToDisabledTag' #-}
