{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Parsing.Parser where

import Prologue hiding (Text)

import qualified Control.Monad.State.Layered                 as State
import qualified Luna.IR                                     as IR
import qualified Luna.Pass                                   as Pass
import qualified Luna.Pass.Attr                              as Attr
import qualified Luna.Pass.Parsing.ExprBuilder               as ExprBuilder
import qualified Luna.Pass.Parsing.Macro                     as Macro
import qualified Luna.Pass.Scheduler                         as Scheduler
import qualified Luna.Syntax.Text.Parser.Data.Name.Hardcoded as Hardcoded
import qualified Luna.Syntax.Text.Parser.IR.Ast              as Parsing (Parser, SyntaxVersion (..))
import qualified Luna.Syntax.Text.Parser.IR.Term             as Parsing
import qualified Luna.Syntax.Text.Parser.Pass                as Parser
import qualified Luna.Syntax.Text.Parser.Pass.Class          as Parser
import qualified Luna.Syntax.Text.Scope                      as Scope

import Data.Text32                         (Text32)
import Luna.Pass                           (Pass)
import Luna.Pass.Parsing.ExprBuilder       (ExprBuilder, ExprBuilderPass)
import Luna.Syntax.Text.Parser.Data.Result (Result (Result))
import Luna.Syntax.Text.Source             (Source (Source))




run :: ExprBuilder.BuilderMonad m => Text32 -> m IR.SomeTerm
run = \s -> do
    let toks = Parser.run Parsing.Syntax1 s
        --FIXME: handle rest of the stream + Right pattern
        Right sect = Macro.runP toks $ do
            Hardcoded.hardcodePrecRelMap
            Macro.parseExpr

    ExprBuilder.buildGraph sect



instance ExprBuilderPass (Pass stage ExprBuilder) => Pass.Definition stage ExprBuilder where
    definition = do
        src  <- unwrap <$> Attr.get @Source
        unit <- run src
        Attr.put $ Result unit


registerDynamic :: ∀ stage m.
    ( ExprBuilderPass (Pass stage ExprBuilder)
    , Scheduler.PassRegister stage ExprBuilder m
    , Scheduler.Monad m
    ) => m ()
registerDynamic = do
    -- Scheduler.registerAttr     @Invalids
    -- Scheduler.enableAttrByType @Invalids
    Scheduler.registerAttr     @Source
    Scheduler.enableAttrByType @Source
    Scheduler.registerAttr     @Result
    Scheduler.enableAttrByType @Result
    Scheduler.registerPass     @stage @ExprBuilder
