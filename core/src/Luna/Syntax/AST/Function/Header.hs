module Luna.Syntax.AST.Function.Header where

import Prelude.Luna

import Luna.Data.Name
import Luna.Syntax.AST.Function.Argument


-- === Definitions === --

data Header a = Named   (Segment a) [Segment a]
              | Unnamed [ArgDef a]
              deriving (Generic, Show, Read, Eq, Ord, Functor, Foldable, Traversable)

data Segment a = Segment Name [ArgDef a]
               deriving (Generic, Show, Read, Eq, Ord, Functor, Foldable, Traversable)

