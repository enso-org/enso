{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Data.Error where

import Prologue

import qualified Data.Construction                  as Data
import qualified Data.Generics.Traversable.Deriving as GTraversable
import qualified Data.Graph.Fold.Class              as FoldClass
import qualified Data.Graph.Fold.Scoped             as Fold
import qualified Data.Graph.Fold.Deep               as Fold
import qualified Data.Graph.Fold.Partition          as Fold
import qualified Data.Graph.Store.Buffer            as Buffer
import qualified Data.Graph.Store.Size.Discovery    as Buffer
import qualified Data.Vector.Storable.Foreign       as Foreign
import qualified Foreign.Storable.Deriving          as Storable
import qualified Luna.Pass.Typing.Data.Target       as Target
import qualified Luna.IR                            as IR
import qualified Luna.IR.Layer                      as Layer

import Data.Graph.Data.Layer.Class  (Layer)
import Luna.Pass.Typing.Data.Target (Target)

data CompileError = CompileError
    { _contents    :: Text
    , _arisingFrom :: [Target]
    , _failedAt    :: [Target]
    } deriving (Show, Eq)
makeLenses ''CompileError

data CompileErrorStore = CompileErrorStore (Foreign.Vector Char)
                                           (Foreign.Vector Target)
                                           (Foreign.Vector Target)
                       deriving (Show)
Storable.deriveNoContext ''CompileErrorStore
GTraversable.derive      ''CompileErrorStore

data Error deriving Generic
instance Layer Error where
    type Cons Error = Layer.Simple (Maybe CompileErrorStore)
    manager = Layer.staticManager

-- FIXME[MK]: These are wrong. They leak memory and cause segfaults when trying to free the layer.
instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep (Fold.Discovery a))) m (Maybe CompileErrorStore)
instance Monad m => FoldClass.Builder Buffer.CopyInitialization2 m (Maybe CompileErrorStore)
instance Monad m => FoldClass.Builder Buffer.CopyInitialization  m (Maybe CompileErrorStore)
instance Monad m => FoldClass.Builder Buffer.Discovery           m (Maybe CompileErrorStore)

errorToStore :: MonadIO m => CompileError -> m CompileErrorStore
errorToStore = \(CompileError c a f) -> do
    contents <- Foreign.fromList $ convert c
    arising  <- Foreign.fromList a
    fail     <- Foreign.fromList f
    pure $ CompileErrorStore contents arising fail
{-# INLINE errorToStore #-}

storeToError :: MonadIO m => CompileErrorStore -> m CompileError
storeToError = \(CompileErrorStore c a f) -> do
    contents <- Foreign.toList c
    arising  <- Foreign.toList a
    fail     <- Foreign.toList f
    pure $ CompileError (convert contents) arising fail
{-# INLINE storeToError #-}

setError ::
    ( Layer.Editor IR.Term Error m
    ) => Maybe CompileError -> IR.Term b -> m ()
setError = \err tgt -> do
    old <- Layer.read @Error tgt
    -- FIXME[MK]: Uncomment when we can safely delete the layer without segfaults.
    {-Data.destructShallow old-}
    new <- traverse errorToStore err
    Layer.write @Error tgt new
{-# INLINE setError #-}

getError ::
    ( Layer.Reader IR.Term Error m
    ) => IR.Term b -> m (Maybe CompileError)
getError = \tgt -> do
    traverse storeToError =<< Layer.read @Error tgt
{-# INLINE getError #-}


unexpected :: CompileError -> CompileError
unexpected = over contents (msg <>) where
    msg = "Unexpected behavior. Please report it as a bug. "

functionNotFound :: IR.Qualified -> IR.Name -> CompileError
functionNotFound mod n = CompileError msg mempty mempty where
    msg =  "Function not found: "
        <> convertVia @String mod <> "." <> convert n

unexpectedFunctionNotFound :: IR.Qualified -> IR.Name -> CompileError
unexpectedFunctionNotFound = unexpected .: functionNotFound

unexpectedConsTypeNotFound ::
    IR.Qualified -> IR.Name -> IR.Name -> CompileError
unexpectedConsTypeNotFound mod cls cons = unexpected err where
    err = CompileError msg mempty mempty
    msg =  "Type of constructor "
        <> convertVia @String mod <> "." <> convert cls <> "." <> convert cons
        <> " could not be resolved."

methodNotFound :: IR.Qualified -> IR.Name -> IR.Name -> CompileError
methodNotFound mod cls n = CompileError msg mempty mempty where
    msg =  "Method "
        <> convert n
        <> " of class "
        <> convertVia @String mod <> "." <> convert cls
        <> " could not be found."

cannotCallMethodOnAFunction :: IR.Name -> CompileError
cannotCallMethodOnAFunction n = CompileError msg mempty mempty where
    msg = "Cannot call method " <> convert n <> " on a function."

cannotUseObjectAsAFunction :: IR.Qualified -> IR.Name -> CompileError
cannotUseObjectAsAFunction mod cls = CompileError msg mempty mempty where
    msg =  "Cannot use object of class "
        <> convertVia @String mod <> "." <> convert cls
        <> " as a function."

unificationError :: Text -> Text -> CompileError
unificationError a b = CompileError msg mempty mempty where
    msg = "Cannot unify " <> a <> " with " <> b <> "."

varNotFound :: IR.Name -> CompileError
varNotFound n = CompileError msg mempty mempty where
    msg = "Variable " <> convert n <> " is not in scope."

consNotFound :: IR.Name -> CompileError
consNotFound n = CompileError msg mempty mempty where
    msg = "Constructor " <> convert n <> " is not in scope."

placeholderError :: CompileError
placeholderError = unexpected $ CompileError "placeholder" mempty mempty
