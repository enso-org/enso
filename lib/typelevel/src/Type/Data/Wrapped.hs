{-# LANGUAGE NoStrict      #-}
{-# LANGUAGE NoStrictData  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeInType    #-}
{-# LANGUAGE TypeOperators #-}

module Type.Data.Wrapped where

import Data.Kind



-- === Basic operations ===

-- FIXME: Useless untill GHC BUG gets resolved: https://ghc.haskell.org/trac/ghc/ticket/14668
--        It just cannot be used outside of this module
type family UnwrapKind (a :: Type) :: Type
type family Unwrap (a :: ka) :: UnwrapKind ka

