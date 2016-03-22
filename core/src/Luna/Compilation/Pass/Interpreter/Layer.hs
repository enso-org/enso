module Luna.Compilation.Pass.Interpreter.Layer where

import           Prologue                        hiding (Getter, Setter)

import           Data.Construction
import           Data.Prop
import           Data.Maybe                      (isJust)

import           Luna.Syntax.Term.Format           (Draft)
import           Luna.Runtime.Dynamics         (Static)
import           Luna.Syntax.Term.Expr            (Term)
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Class (Network)
import qualified Luna.Syntax.Model.Network.Term  as Term

import           GHC.Prim                        (Any)


type EvalMonad = IO
evalMonad = "IO"

data InterpreterLayer = InterpreterLayer { _dirty    :: Bool
                                         , _required :: Bool
                                         , _value    :: Maybe (EvalMonad Any)
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
