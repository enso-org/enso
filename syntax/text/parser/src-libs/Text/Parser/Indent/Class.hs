module Text.Parser.Indent.Class where

import Data.Text.Position
import Prologue



--------------------
-- === Indent === --
--------------------

data Indent = Indent { _level :: Delta
                     , _stack :: [Delta]
                     } deriving (Show)
makeLenses ''Indent


-- === Modification === --

push :: Delta -> Indent -> Indent
push d i = i & stack %~ (i ^. level :)
             & level .~ d

pop :: Indent -> (Delta, Indent)
pop i = (i ^. level,) $ i & level .~ unsafeHead (i ^. stack)
                          & stack %~ unsafeTail


-- === Instances === --

instance Default Indent where
    def = Indent def def
