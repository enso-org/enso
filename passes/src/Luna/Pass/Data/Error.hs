{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Data.Error where

import Prologue

import qualified Luna.IR                      as IR

data DefSource = FromMethod IR.Name IR.Name
               | FromFunction IR.Name
               deriving (Show, Eq)

data ErrorSource = ErrorSource
    { _modName :: IR.Qualified
    , _defSrc  :: DefSource
    } deriving (Show, Eq)

data CompileError = CompileError
    { _contents    :: Text
    , _arisingFrom :: [ErrorSource]
    } deriving (Show, Eq)

functionNotFound :: IR.Qualified -> IR.Name -> CompileError
functionNotFound mod n = CompileError msg mempty where
    msg =  "Function not found: "
        <> convertVia @String mod
        <> "."
        <> convert n

methodNotFound :: IR.Qualified -> IR.Name -> IR.Name -> CompileError
methodNotFound mod cls n = CompileError msg mempty where
    msg =  "Function not found: "
        <> convertVia @String mod
        <> "."
        <> convert cls
        <> "."
        <> convert n

placeholderError :: CompileError
placeholderError = CompileError "placeholder" mempty
