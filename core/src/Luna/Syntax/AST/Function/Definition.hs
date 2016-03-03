module Luna.Syntax.AST.Function.Definition where

import Prelude.Luna

import qualified Luna.Syntax.AST.Function.Signature as Signature


data Def header body = Def { __header_ :: header
                           , __body_   :: body
                           } deriving (Show, Functor, Foldable, Traversable)


type Function a body = Def (Signature.Function a) body
type Method   a body = Def (Signature.Method   a) body
