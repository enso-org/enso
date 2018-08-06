{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Syntax.Text.Lexer.Runner where

import Prologue hiding (Symbol)

import qualified Control.Monad.State.Layered    as State
import qualified Data.Attoparsec.Internal.Types as Parser
import qualified Data.Text32                    as Text32
import qualified Luna.Syntax.Text.Lexer.Grammar as Grammar
import qualified Luna.Syntax.Text.Lexer.Symbol  as Symbol
import qualified Text.Parser.State.Indent       as State.Indent

import Data.Attoparsec.Internal.Types (IResult)
import Data.Parser                    (PartialParser, closePartial,
                                       parsePartial)
import Data.Text.Position             (Delta)
import Data.Text32                    (Text32)
import Luna.Syntax.Text.Lexer.Grammar (Parser, lexer)
import Luna.Syntax.Text.Lexer.Symbol  (Symbol)
import Luna.Syntax.Text.Lexer.Token   (Token, token)

import Data.Parser.Instances.Attoparsec ()



---------------------
-- === Running === --
---------------------

evalDefLexer :: Text32 -> [Token]
evalDefLexer = \s ->
    let s'  = Text32.dropWhile (== ' ') s
        off = Text32.length s - Text32.length s'
        go  = reverse . evalLexer__
    in  stx off : go s'
{-# INLINE evalDefLexer #-}

evalLexer__ :: Text32 -> [Token]
evalLexer__ = \txt -> case runner__ txt of
    Parser.Done !(txt') !r -> if (Text32.length txt' /= 0)
        then error $ "Panic. Not all input consumed by lexer: " <> show txt'
        else unwrap r
    Parser.Partial g -> case g mempty of
        Parser.Done !(txt') !r -> if (Text32.length txt' /= 0)
            then error $ "Panic. Not all input consumed by lexer: " <> show txt'
            else unwrap r
        Parser.Fail !_ !_ !e   -> error e
        Parser.Partial {} -> impossible

    Parser.Fail !_ !_ !e   -> error e
{-# INLINE evalLexer__ #-}

runner__ :: Text32 -> IResult Text32 Grammar.Result
runner__ = \txt
    -> flip parsePartial (txt <> "\ETX")
     $ flip (State.evalT @Grammar.Location) mempty
     $ State.Indent.eval
     $ flip (State.execT  @Grammar.Result) mempty
     $ lexer
{-# INLINE runner__ #-}


-- === STX / ETX handling === --

class IsSourceBorder a where
    stx :: Int -> a
    etx :: a

instance IsSourceBorder r => IsSourceBorder (Either l r) where
    stx = Right . stx
    etx = Right   etx
    {-# INLINE stx #-}
    {-# INLINE etx #-}

instance IsSourceBorder Token where
    stx i = token mempty (convert i) 0 0 True (stx i)
    etx   = token mempty mempty      0 0 True etx
    {-# INLINE stx #-}
    {-# INLINE etx #-}

instance IsSourceBorder Symbol where
    stx _ = Symbol.STX
    etx   = Symbol.ETX
    {-# INLINE stx #-}
    {-# INLINE etx #-}

-- instance IsSourceBorder (Symbol, EntryStack) where
--     stx i = (stx i, mempty)
--     etx   = (etx, mempty)
--     {-# INLINE stx #-}
--     {-# INLINE etx #-}
