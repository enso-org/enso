{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.ErrorPropagation where

import Prologue

import qualified Data.Graph.Data.Component.List        as ComponentList
import qualified Data.Graph.Data.Component.Vector      as ComponentVector
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Store                      as Store
import qualified Data.Mutable.Class                    as Mutable
import qualified Data.Set                              as Set
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Data.Layer.Requester        as Requester
import qualified Luna.Pass.Data.Stage                  as TC
import qualified Luna.Pass.Data.UniqueNameGen          as NameGen
import qualified Luna.Pass.Typing.Base                 as TC
import qualified Luna.Pass.Data.Error                  as Error
import qualified Luna.Syntax.Prettyprint               as Prettyprint


import Luna.Pass.Data.Root (Root (..))


data ErrorPropagation

type instance Pass.Spec ErrorPropagation t = ErrorPropagationSpec t
type family ErrorPropagationSpec t where
    ErrorPropagationSpec (Pass.In  Pass.Attrs) = '[Root]
    ErrorPropagationSpec (Pass.Out Pass.Attrs) = '[]
    ErrorPropagationSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage ErrorPropagation where
    definition = do
        Root root <- Attr.get
        invalidErrors root
        propagateErrors root

invalidErrors :: IR.SomeTerm -> TC.Pass ErrorPropagation ()
invalidErrors expr = do
    Layer.read @IR.Model expr >>= \case
        Uni.Invalid inv -> do
            users <- Mutable.toList =<< Layer.read @IR.Users expr
            case users of
                [user] -> do
                    userStr <- Prettyprint.run @Prettyprint.Simple def
                        =<< IR.target user
                    Error.setError (Just $ Error.syntaxError inv (Just userStr)) expr
                _      -> Error.setError (Just $ Error.syntaxError inv Nothing) expr
        _ -> do
            inputs    <- ComponentList.mapM IR.source =<< IR.inputs expr
            traverse_ invalidErrors inputs

propagateErrors :: IR.SomeTerm -> TC.Pass ErrorPropagation ()
propagateErrors expr = do
    let updateErrors e = do
            ownErr <- Error.getError e
            when (isNothing ownErr) $ do
                inputs    <- ComponentList.mapM IR.source =<< IR.inputs e
                inpErrors <- head . catMaybes <$> traverse getErrors inputs
                Error.setError inpErrors e
    Layer.read @IR.Model expr >>= \case
        Uni.Seq a b -> do
            propagateErrors =<< IR.source a
            propagateErrors =<< IR.source b
        Uni.Function _ _ b -> do
            propagateErrors =<< IR.source b
            updateErrors expr
        Uni.Marked _ e -> do
            propagateErrors =<< IR.source e
            updateErrors expr
        Uni.Unify a b -> do
            propagateErrors =<< IR.source a
            propagateErrors =<< IR.source b
            updateErrors expr
            ownErr <- Error.getError expr
            Error.setError ownErr =<< IR.source a
        _ -> do
            updateErrors expr

getErrors :: IR.SomeTerm -> TC.Pass ErrorPropagation (Maybe Error.CompileError)
getErrors expr = do
    ownErr <- Error.getError expr
    case ownErr of
        Just e  -> pure $ Just e
        Nothing -> do
            inputs <- ComponentList.mapM IR.source =<< IR.inputs expr
            head . catMaybes <$> traverse getErrors inputs

