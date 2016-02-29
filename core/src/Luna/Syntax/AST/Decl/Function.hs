{-# LANGUAGE TypeOperators #-}

module Luna.Syntax.AST.Decl.Function where

import Prologue
import Luna.Syntax.AST.Arg


-- === Definitions === --

data Signature a = Signature { _self  :: Maybe a
                             , _args  :: [Arg a]
                             , _out   :: a
                             } deriving (Show, Functor, Traversable, Foldable)

data Function a b = Function { _sig  :: Signature a
                             , _body :: b
                             } deriving (Show, Functor, Traversable, Foldable)

makeLenses ''Signature
makeLenses ''Function


-- === Instances === --

-- Castable
instance Castable n n' => Castable (Signature n) (Signature n') where
    cast = fmap cast ; {-# INLINE cast #-}
