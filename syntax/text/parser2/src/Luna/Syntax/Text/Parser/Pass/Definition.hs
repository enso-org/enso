{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Syntax.Text.Parser.Pass.Definition where

import Prologue

import qualified Control.Monad.State.Layered                as State
import qualified Data.Attoparsec.Internal.Types             as Parser
import qualified Data.Graph.Component.Node.Construction     as Term
import qualified Data.Text32                                as Text32
import qualified Luna.IR                                    as IR
import qualified Luna.Pass                                  as Pass
import qualified Luna.Pass.Attr                             as Attr
import qualified Luna.Pass.Scheduler                        as Scheduler
import qualified Luna.Syntax.Text.Lexer                     as Lexer
import qualified Luna.Syntax.Text.Lexer.Symbol              as Symbol
import qualified Luna.Syntax.Text.Parser.Data.Ast           as Ast
import qualified Luna.Syntax.Text.Parser.Lexer              as Parsing
import qualified Luna.Syntax.Text.Parser.Parser.ExprBuilder as ExprBuilder
import qualified Luna.Syntax.Text.Parser.State.Marker       as Marker
import qualified Luna.Syntax.Text.Parser.State.TokenStream  as TokenStream
import qualified Text.Megaparsec                            as Parser
import qualified Text.Parser.State.Indent                   as State.Indent

import Data.Attoparsec.Internal.Types              (IResult)
import Data.Parser                                 (PartialParser, closePartial,
                                                    parsePartial, token)
import Data.Text.Position                          (FileOffset, Position)
import Data.Text32                                 (Text32)
import Luna.IR                                     (SomeTerm)
import Luna.Pass                                   (Pass)
import Luna.Syntax.Text.Parser.Data.CodeSpan       (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Name.Hardcoded (hardcode)
-- import Luna.Syntax.Text.Parser.Data.Result         (Result (Result))
-- import Luna.Syntax.Text.Parser.IR.Class            (Error, ParserBase, Stream,
                                                    -- Token)
import Luna.Syntax.Text.Parser.Lexer (Token)
-- import Luna.Syntax.Text.Parser.Pass.Class          (IRB (fromIRB), IRBS,
--                                                     ParserPass, fromIRBS)
import Luna.Syntax.Text.Parser.State.LastOffset (LastOffset)
import Luna.Syntax.Text.Parser.State.Reserved   (Reserved)
import Luna.Syntax.Text.Scope                   (Scope)
import Luna.Syntax.Text.Source                  (Source)
import Text.Megaparsec                          (ParseError, ParsecT)
import Text.Megaparsec.Error                    (parseErrorPretty)
import Text.Parser.State.Indent                 (Indent)





runStack :: Parsing.SyntaxVersion -> Parsing.Lexer a -> Text32 -> IResult Text32 [Token]
runStack = \sv p txt
    -> flip parsePartial txt -- (txt <> "\ETX")
     $ State.evalDefT @Scope
     $ State.evalDefT @FileOffset
     $ State.evalDefT @Marker.State
     $ State.evalDefT @LastOffset
     $ State.evalDefT @Position
     $ State.Indent.eval
     $ flip (State.evalT @Parsing.SyntaxVersion) sv
     $ TokenStream.eval p
{-# INLINE runStack #-}


evalTotal :: Parsing.SyntaxVersion -> Parsing.Lexer a -> Text32 -> [Token]
evalTotal = \sv p txt -> case runStack sv p txt of
    Parser.Done !(txt') !r -> if (Text32.length txt' /= 0)
        then error $ "Panic. Not all input consumed by lexer: " <> show txt'
        else r
    Parser.Partial g -> case g mempty of
        Parser.Done !(txt') !r -> if (Text32.length txt' /= 0)
            then error $ "Panic. Not all input consumed by lexer: " <> show txt'
            else r
        Parser.Fail !_ !_ !e   -> error e
        Parser.Partial {} -> impossible

    Parser.Fail !_ !_ !e   -> error e
{-# INLINE evalTotal #-}


run :: Parsing.SyntaxVersion -> Text32 -> [Token]
run = \sv txt -> evalTotal sv Parsing.exprs (txt <> "\ETX")
{-# INLINE run #-}
