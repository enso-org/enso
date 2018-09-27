{-# LANGUAGE Strict               #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}

module Luna.Pass.Data.Error where

import Prologue hiding (intercalate)

import qualified Control.Monad.State.Layered           as State
import qualified Data.Construction                     as Data
import qualified Data.Generics.Traversable.Deriving    as GTraversable
import qualified Data.Graph.Fold.Class                 as FoldClass
import qualified Data.Graph.Fold.Scoped                as Fold
import qualified Data.Graph.Fold.Deep                  as Fold
import qualified Data.Graph.Fold.Partition             as Fold
import qualified Data.Graph.Store.Buffer               as Buffer
import qualified Data.Graph.Store.Size.Discovery       as Buffer
import qualified Data.Mutable.Class                    as Mutable
import qualified Foreign.Storable.Deriving             as Storable
import qualified Luna.Pass.Typing.Data.Target          as Target
import qualified Luna.Pass.Resolve.Data.Resolution     as Resolution
import qualified Luna.IR                               as IR
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.IR.Term.Ast.Invalid              as Invalid

import Data.Graph.Data.Layer.Class           (Layer)
import Data.Mutable.Storable.SmallAutoVector (UnmanagedSmallVector)
import Data.Text                             (intercalate)
import Luna.Pass.Typing.Data.Target          (Target)

data CompileError = CompileError
    { _contents    :: Text
    , _arisingFrom :: [Target]
    , _failedAt    :: [Target]
    } deriving (Show, Eq)
makeLenses ''CompileError

data CompileErrorStore = CompileErrorStore (UnmanagedSmallVector 0 Char)
                                           (UnmanagedSmallVector 4 Target)
                                           (UnmanagedSmallVector 4 Target)
                       deriving (Show)
Storable.deriveNoContext ''CompileErrorStore
GTraversable.derive      ''CompileErrorStore

instance MonadIO m
      => FoldClass.Builder Buffer.CopyInitialization2 m CompileErrorStore where
    build = \(CompileErrorStore x y z) a
        -> FoldClass.build @Buffer.CopyInitialization2 x
         $ FoldClass.build @Buffer.CopyInitialization2 y
         $ FoldClass.build @Buffer.CopyInitialization2 z
         $ a
    {-# INLINE build #-}

instance (State.Monad Buffer.StoreDynState m, MonadIO m)
      => FoldClass.Builder Buffer.CopyInitialization m CompileErrorStore where
    build = \(CompileErrorStore x y z) a
        -> FoldClass.build @Buffer.CopyInitialization x
         $ FoldClass.build @Buffer.CopyInitialization y
         $ FoldClass.build @Buffer.CopyInitialization z
         $ a
    {-# INLINE build #-}

instance MonadIO m
      => FoldClass.Builder Buffer.Discovery m CompileErrorStore where
    build = \(CompileErrorStore x y z) a
        -> FoldClass.build @Buffer.Discovery x
         $ FoldClass.build @Buffer.Discovery y
         $ FoldClass.build @Buffer.Discovery z
         $ a
    {-# INLINE build #-}

instance Monad m => FoldClass.Builder (Fold.Scoped (Fold.Deep
                                          (Fold.Discovery
                                              '[IR.Terms, IR.Links])))
                        m (Maybe CompileErrorStore)

data Error deriving Generic
instance Layer Error where
    type Cons Error = Layer.Simple (Maybe CompileErrorStore)
    manager = Layer.dynamicManager

errorToStore :: MonadIO m => CompileError -> m CompileErrorStore
errorToStore = \(CompileError c a f) -> do
    contents <- Mutable.fromList $ convert c
    arising  <- Mutable.fromList a
    fail     <- Mutable.fromList f
    pure $ CompileErrorStore contents arising fail
{-# INLINE errorToStore #-}

storeToError :: MonadIO m => CompileErrorStore -> m CompileError
storeToError = \(CompileErrorStore c a f) -> do
    contents <- Mutable.toList c
    arising  <- Mutable.toList a
    fail     <- Mutable.toList f
    pure $ CompileError (convert contents) arising fail
{-# INLINE storeToError #-}

setError ::
    ( Layer.Editor IR.Term Error m
    ) => Maybe CompileError -> IR.Term b -> m ()
setError = \err tgt -> do
    old <- Layer.read @Error tgt
    Data.destructShallow old
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

duplicateFunctionDefinition :: IR.Qualified -> IR.Name -> CompileError
duplicateFunctionDefinition mod n = CompileError msg mempty mempty where
    msg =  "Duplicate function definition: "
        <> convert n <> " in module " <> convertVia @String mod

duplicateClassDefinition :: IR.Qualified -> IR.Name -> CompileError
duplicateClassDefinition mod cls = CompileError msg mempty mempty where
    msg =  "Duplicate class definition: "
        <> convert cls <> " in module " <> convertVia @String mod

duplicateMethodDefinition :: IR.Qualified -> IR.Name -> IR.Name -> CompileError
duplicateMethodDefinition mod cls n = CompileError msg mempty mempty where
    msg =  "Duplicate method definition: "
        <> convert n <> " in class " <> convert cls
        <> " in module " <> convertVia @String mod

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

consAmbiguous :: IR.Name -> [Resolution.ConsRef] -> CompileError
consAmbiguous n consRefs = CompileError msg mempty mempty where
    msg = "Constructor " <> convert n <> " is ambiguous. "
       <> "It is defined in classes: "
       <> intercalate ", " (map showRef consRefs)
    showRef (Resolution.ConsRef unitName className _) =
        convertVia @String unitName <> "." <> convert className

placeholderError :: CompileError
placeholderError = unexpected $ CompileError "placeholder" mempty mempty

syntaxError :: Invalid.Symbol -> Maybe Text -> CompileError
syntaxError inv expr = CompileError msg mempty mempty where
    msg = "Syntax error: " <> convert (show inv)
        <> maybe "" (\e -> " in expression `" <> e <> "`") expr
