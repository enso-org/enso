module Luna.Interpreter.Layer where

import           Prologue                        hiding (Getter, Setter)

import           Data.Construction
import           Old.Data.Prop
import           Data.Either                     (isRight)

import           Luna.Syntax.Model.Layer
import           Luna.Interpreter.Value



type EvalMonad = IO

evalMonad :: String
evalMonad = "IO"


type Errors = [String]

type ValueErr a = Either Errors a

data InterpreterLayer = InterpreterLayer { _dirty    :: Bool
                                         , _required :: Bool
                                         , _value    :: ValueErr Value
                                         , _time     :: Integer
                                         , _debug    :: String     -- debug data (String value)
                                         , _binders  :: Int
                                         }
makeLenses ''InterpreterLayer

instance Default InterpreterLayer where
    def = InterpreterLayer True False (Left []) 0 "" 0

instance Show InterpreterLayer where
    show (InterpreterLayer dirty required value time _ _) = "InterpreterLayer{"
        <> show dirty <> ","
        <> show required <> ","
        <> valueStr <> ","
        <> show time <> "}" where
            valueStr = case value of
                           Left err -> "Left " <> show err
                           Right _  -> "Right Any"

data InterpreterData = InterpreterData deriving (Show, Eq, Ord)

type instance LayerData InterpreterData a = InterpreterLayer

instance Monad m => Creator    m (Layer InterpreterData a) where create = return $ Layer def
instance Monad m => Destructor m (Layer InterpreterData t) where destruct _ = return ()

instance Castable InterpreterLayer InterpreterLayer where cast = id ; {-# INLINE cast #-}
