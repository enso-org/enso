module Prologue.Text.Pretty where

import Data.Text (Text)

-------------------
-- === Class === --
-------------------

class PrettyShow a where
    prettyShow :: a -> Text

