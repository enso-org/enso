module Luna.IR.Function.Class where

import Luna.Prelude

import Luna.IR.Function.Argument


data Method   a body = Method   { __self_ :: a
                                , __func_ :: Function a body
                                } deriving (Show, Functor, Foldable, Traversable)

data Function a body = Function { __args_ :: [ArgDef a]
                                , __out_  :: body
                                } deriving (Show, Functor, Foldable, Traversable)
