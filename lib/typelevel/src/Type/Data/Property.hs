{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Data.Property where

import Data.Kind
import Prelude
import Type.Data.Maybe



-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- THE CORRECT POLYMORPHIC DEFINITION IS IN Type.Data.Map BECAUSE OF GHC BUG:
-- https://ghc.haskell.org/trac/ghc/ticket/14668
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


----------------------
-- === Property === --
----------------------

-- NOTE: The order of arguments has to be this if we want to make it
--       type and kind dependent. GHC does not allow us to reorder them then.


-- | Query 'obj' of any kind by 'key' to get 'val'. The dependencies
--   'obj' -> 'key kind' and 'obj' -> 'val kind' are provided by
--   'KeyKind' and 'ValKind' respectively. See Type.Data.Map as usage reference.
type family (obj :: a) !? (key :: Type) :: Maybe Type

-- | The '!!' query operator is exactly like '!?' but returns the result
--   directly instead of encoding it in Maybe. It raises compilation error
--   if the key is missing.
type family (obj :: a) !! (key :: Type) :: Type where
    obj !! key = FromJust (obj !? key)

type family Set (obj :: a) (key :: Type) (val :: Type) :: a

