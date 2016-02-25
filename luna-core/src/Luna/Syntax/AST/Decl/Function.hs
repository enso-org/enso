{-# LANGUAGE TypeOperators #-}

module Luna.Syntax.AST.Decl.Function where

import Prologue
import Data.Graph.Backend.VectorGraph


data FunctionPtr n = FunctionPtr { _self  :: Maybe (Ref Node n)
                                 , _args  :: [Ref Node n]
                                 , _out   :: Ref Node n
                                 , _tpRep :: Ref Node n
                                 } deriving (Show)
makeLenses ''FunctionPtr

instance Castable n n' => Castable (FunctionPtr n) (FunctionPtr n') where
    cast (FunctionPtr s a o t) = FunctionPtr (fmap cast s) (fmap cast a) (cast o) (cast t)

data Function n g = Function { _fptr  :: FunctionPtr n
                             , _graph :: g
                             } deriving (Show)
makeLenses ''Function
