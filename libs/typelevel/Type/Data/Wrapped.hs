{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeInType         #-}

module Type.Data.Wrapped where

import Data.Typeable
import Prelude
import Data.Kind
import GHC.Exts (Constraint)


-- === Basic operations ===

-- FIXME: Useless untill GHC BUG gets resolved: https://ghc.haskell.org/trac/ghc/ticket/14668
--        It just cannot be used outside of this module
type family UnwrapKind (a :: Type) :: Type
type family Unwrap (a :: ka) :: UnwrapKind ka
