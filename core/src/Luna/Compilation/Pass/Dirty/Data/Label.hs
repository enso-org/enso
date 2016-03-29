module Luna.Compilation.Pass.Dirty.Data.Label where

import           Luna.Compilation.Pass.Dirty.Data.CallPointPath (CallPointPath)

import           Data.Construction
import           Luna.Syntax.Model.Layer
import           Prologue                                       hiding (Getter, Setter)

import           Data.Prop
import           Luna.Syntax.Term.Format                          (Draft)
import           Luna.Syntax.Model.Network.Class                (Network)
import qualified Luna.Syntax.Model.Network.Term                 as Term

import           Luna.Runtime.Dynamics                        (Static)
import           Luna.Syntax.Term.Expr                           (Term)


data InterpreterLayer = InterpreterLayer { _dirty    :: Bool
                                         , _required :: Bool
                                         , _value    :: Maybe Int
                                         -- , _userNode :: Bool
                                         -- , _location :: CallPointPath
                                         } deriving Show

makeLenses ''InterpreterLayer

instance Default InterpreterLayer where
    def = InterpreterLayer True False Nothing

data Interpreter = Interpreter deriving (Show, Eq, Ord)

type instance LayerData Interpreter t = InterpreterLayer

instance Monad m => Creator    m (Layer Interpreter a) where create = return $ Layer def
instance Monad m => Destructor m (Layer Interpreter t) where destruct _ = return ()

instance Castable InterpreterLayer InterpreterLayer where cast = id ; {-# INLINE cast #-}
