module Luna.Syntax.Text.Source where

import Prologue
import Data.Text.Position (Delta)
import Luna.IR (SomeExpr)
import qualified Data.VectorText as VectorText
import           Data.VectorText (VectorText)

--------------------
-- === Source === --
--------------------

-- === Definition === --

newtype Source = Source VectorText deriving (Show)
makeLenses ''Source


-- === Instances === --

instance IsString               Source     where fromString = convert
instance Convertible String     Source     where convert = convertVia @VectorText
instance Convertible VectorText Source     where convert = wrap
instance Convertible Source     VectorText where convert = unwrap
instance Convertible Source     String     where convert = convertVia @VectorText
