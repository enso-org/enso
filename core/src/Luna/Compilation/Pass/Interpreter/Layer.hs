module Luna.Compilation.Pass.Interpreter.Layer where

import           Prologue                        hiding (Getter, Setter)

import           Data.Construction
import           Data.Prop

import           Luna.Evaluation.Model           (Draft)
import           Luna.Evaluation.Runtime         (Static)
import           Luna.Syntax.AST.Term            (Term)
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Class (Network)
import qualified Luna.Syntax.Model.Network.Term  as Term


data InterpreterLayer = InterpreterLayer { _dirty    :: Bool
                                         , _required :: Bool
                                         , _value    :: Maybe Int
                                         } deriving Show

makeLenses ''InterpreterLayer

instance Default InterpreterLayer where
    def = InterpreterLayer True False Nothing

data InterpreterData = InterpreterData deriving (Show, Eq, Ord)

type instance LayerData layout InterpreterData t = InterpreterLayer

instance Monad m => Creator    m (Layer layout InterpreterData a) where create = return $ Layer def
instance Monad m => Destructor m (Layer layout InterpreterData t) where destruct _ = return ()

instance Castable InterpreterLayer InterpreterLayer where cast = id ; {-# INLINE cast #-}
