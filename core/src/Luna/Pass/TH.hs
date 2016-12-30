{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType #-}
-- {-# LANGUAGE CPP #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE NoRecursiveDo #-}

module Luna.Pass.TH where

import Prologue
-- import Language.Haskell.TH hiding(cxt)
-- import qualified Data.Set as Set
-- import Control.Monad
-- import Data.Monoid
-- import Prelude

import Data.Path
import Type.List (type(<>))
--
-- makeEventPass :: Name -> Q [Dec]
-- makeEventPass name = do
--     r <- reify name
--     runIO $ print    $ r
--     runIO $ putStrLn $ pprint r
--     return []


type family Prep a (ls :: [*]) :: [*] where
    Prep (Proxy a) '[]       = '[]
    Prep (Proxy a) (l ': ls) = (a // l) ': Prep (Proxy a) ls

type family Expand ls :: [*] where
    Expand (Proxy (l // ls))   = Prep (Proxy l) (Expand (Proxy ls))
    -- Expand (l // ls) = Permute l (Expand ls)
    Expand (Proxy (ls :: [k])) = Expands (Proxy ls)
    Expand (Proxy (ls ::  *))  = '[ls]


type family Expands (ls :: *) :: [*] where
    Expands (Proxy '[])       = '[]
    Expands (Proxy (l ': ls)) = Expand (Proxy l) <> Expands (Proxy ls)
