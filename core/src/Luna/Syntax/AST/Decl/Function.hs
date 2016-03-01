{-# LANGUAGE TypeOperators #-}

module Luna.Syntax.AST.Decl.Function where

import Prologue
import Luna.Syntax.AST.Arg
import Luna.Syntax.Ident.Pattern


-- === Definitions === --

data Signature2 a = Signature2 { __self_ :: Maybe a
                               , __pat_  :: Pattern a
                               , __out_  :: a
                               } deriving (Show, Functor, Foldable, Traversable)

data Function2 a b = Function2 { __sig_  :: Signature2 a
                               , __body_ :: b
                               } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Signature2
makeLenses ''Function2



data Signature a = Signature { _self  :: Maybe a
                             , _args  :: [Arg a]
                             , _out   :: a
                             } deriving (Show, Functor, Foldable, Traversable)

data Function a b = Function { _sig  :: Signature a
                             , _body :: b
                             } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Signature
makeLenses ''Function


-- === Instances === --

-- Castable
instance Castable n n' => Castable (Signature n) (Signature n') where
    cast = fmap cast ; {-# INLINE cast #-}
