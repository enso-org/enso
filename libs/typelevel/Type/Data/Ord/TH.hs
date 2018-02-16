{-# LANGUAGE ViewPatterns #-}
module Type.Data.Ord.TH where


import Prologue hiding (Type)

import           Data.List                   (subsequences)
import qualified Language.Haskell.TH         as TH
import           Language.Haskell.TH         (Type, Q, Name)
import           Language.Haskell.TH.Builder

import           Type.Data.Ord


pairs :: [a] -> [(a, a)]
pairs = map (\[x, y] -> (x, y)) . filter ((== 2) . length) . subsequences

lt, gt, eq :: TH.Type
lt = var 'LT
gt = var 'GT
eq = var 'EQ

defLT :: Name -> Name -> TH.Dec
defLT (var -> tlt) (var -> tgt) = typeInstance2 ''Cmp tlt tgt lt

defGT :: Name -> Name -> TH.Dec
defGT (var -> tlt) (var -> tgt) = typeInstance2 ''Cmp tgt tlt gt

defEQ :: Name -> TH.Dec
defEQ (var -> t) = typeInstance2 ''Cmp t t eq

-- | Generate an instance of the `Cmp` family to impose the following ordering:
--   > type instance Cmp tlt tgt = LT
--   > type instance Cmp tgt tlt = GT
--   > type instance Cmp tlt tgt = EQ
--   > type instance Cmp tgt tlt = EQ
--
--   which is equivalent to saying: tlt < tgt
defOrderedPair :: Name -> Name -> [TH.Dec]
defOrderedPair tlt tgt = [defLT tlt tgt, defGT tlt tgt, defEQ tlt, defEQ tgt]

-- | Define a total ordering on the list of types.
--   [T1, T2, ..., Tn] => T1 < T2 < ... < Tn
defOrder :: [Name] -> Q [TH.Dec]
defOrder = return . concatMap (uncurry defOrderedPair) . pairs
