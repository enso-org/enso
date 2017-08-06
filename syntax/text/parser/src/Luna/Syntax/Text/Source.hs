module Luna.Syntax.Text.Source where

import Prologue
import Data.Text.Position (Delta)
import Luna.IR (SomeExpr)


--------------------
-- === Source === --
--------------------

-- === Definition === --

newtype Source = Source Text deriving (Show)
makeLenses ''Source


-- === Instances === --

instance IsString           Source where fromString = convert
instance Convertible String Source where convert    = convertVia @Text
instance Convertible Text   Source where convert    = coerce
instance Convertible Source Text   where convert    = coerce
instance Convertible Source String where convert    = convertVia @Text
