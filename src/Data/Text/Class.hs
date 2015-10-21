
module Data.Text.Class where

import Prelude
import Data.Text.Lazy (Text, pack, unpack)


------------------------------------------------------------------------
-- Type classes
------------------------------------------------------------------------

class ToText a where
    toText :: a -> Text

class FromText a where
    fromText :: Text -> a

class (ToText a, FromText a) => IsText a


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance ToText Text where
    toText = id

instance FromText Text where
    fromText = id

instance IsText Text

---

instance ToText String where
    toText = pack

instance FromText String where
    fromText = unpack

instance IsText String

