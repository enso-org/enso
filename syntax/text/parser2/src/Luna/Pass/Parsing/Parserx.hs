{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.Parserx where

import Prologue hiding (Text)

import qualified Control.Monad.State.Layered                 as State
import qualified Luna.IR                                     as IR
import qualified Luna.Pass                                   as Pass
import qualified Luna.Pass.Attr                              as Attr
import qualified Luna.Pass.Scheduler                         as Scheduler
import qualified Luna.Syntax.Text.Parser.Data.Name.Hardcoded as Hardcoded
import qualified Luna.Syntax.Text.Parser.Lexer               as Lexer
import qualified Luna.Syntax.Text.Parser.Parser              as Macro
import qualified Luna.Syntax.Text.Parser.Parser.ExprBuilder  as ExprBuilder
import qualified Luna.Syntax.Text.Scope                      as Scope

import Data.Text32 (Text32)
import Luna.Pass   (Pass)
-- import Luna.Syntax.Text.Parser.Data.Result (Result (Result))
import Luna.Syntax.Text.Source (Source (Source))





run :: Text32 -> Lexer.Token
run = runWith Macro.unit
{-# NOINLINE run #-}

runWith :: Macro.Parser a -> Text32 -> a
runWith = \p src -> let
    toks = Lexer.eval Lexer.Syntax1 src
    Right out = Macro.run toks $ do
        Hardcoded.hardcodePrecRelMap
        Macro.hardcodePredefinedMacros
        p
    in out
{-# INLINE runWith #-}

