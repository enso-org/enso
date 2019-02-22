
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif

module Data.String.Class (
    module Data.String.Class,
    module X
) where

import Prelude
import Data.String as X (IsString (fromString))
import qualified Data.Text.Lazy as LText
import qualified Data.Text      as Text


-- === Types ===

class ToString a where
    toString   :: a -> String

    default toString :: Show a => a -> String
    toString = show


-- === Instances ===

instance ToString String where
    toString = id

instance ToString LText.Text where
    toString = LText.unpack

instance ToString Text.Text where
    toString = Text.unpack

instance {-# OVERLAPPABLE #-} Show a => ToString a where
    toString = show

