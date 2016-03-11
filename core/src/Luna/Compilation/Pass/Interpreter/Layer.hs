module Luna.Compilation.Pass.Interpreter.Layer where

import           Prologue                        hiding (Getter, Setter)

import           Data.Construction
import           Data.Prop
import           Data.Maybe                      (isJust)

import           Luna.Evaluation.Model           (Draft)
import           Luna.Evaluation.Runtime         (Static)
import           Luna.Syntax.AST.Term            (Term)
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Class (Network)
import qualified Luna.Syntax.Model.Network.Term  as Term

import           GHC.Prim                        (Any)

data InterpreterLayer = InterpreterLayer { _dirty    :: Bool
                                         , _required :: Bool
                                         , _value    :: Maybe Any
                                         , _time     :: Integer
                                         , _debug    :: String     -- debug data (String value)
                                         }
makeLenses ''InterpreterLayer

instance Default InterpreterLayer where
    def = InterpreterLayer True False Nothing 0 ""

instance Show InterpreterLayer where
    show (InterpreterLayer dirty required value time _) = "InterpreterLayer{"
        <> show dirty <> ","
        <> show required <> ","
        <> if isJust value then "Just Any" else "Nothing" <> ","
        <> show time <> "}"

data InterpreterData = InterpreterData deriving (Show, Eq, Ord)

type instance LayerData layout InterpreterData t = InterpreterLayer

instance Monad m => Creator    m (Layer layout InterpreterData a) where create = return $ Layer def
instance Monad m => Destructor m (Layer layout InterpreterData t) where destruct _ = return ()

instance Castable InterpreterLayer InterpreterLayer where cast = id ; {-# INLINE cast #-}
