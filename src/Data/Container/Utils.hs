module Data.Container.Utils where

-- FIXME: This module shouldn't be here. This code should be in a separate library,
-- so that we can import it both here and in Prologue.

import Prelude

infixr 8 .:
(.:) :: (a -> b) -> (c1 -> c2 -> a) -> c1 -> c2 -> b
(.:) = (.) . (.)
