{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Type.Data.Ord.TH where

import Prelude

import qualified Language.Haskell.TH as TH

import Data.List                   (subsequences)
import Language.Haskell.TH         (Name, Q)

import Language.Haskell.TH.Builder
import Type.Data.Ord.Class


pairs :: [a] -> [(a, a)]
pairs = fmap (\[x, y] -> (x, y)) . filter ((== 2) . length) . subsequences

lt, gt, eq :: TH.Type
lt = cons' 'LT
gt = cons' 'GT
eq = cons' 'EQ

defLT :: Name -> Name -> TH.Dec
defLT (cons' -> tlt) (cons' -> tgt) = typeInstance2 ''Cmp tlt tgt lt

defGT :: Name -> Name -> TH.Dec
defGT (cons' -> tlt) (cons' -> tgt) = typeInstance2 ''Cmp tgt tlt gt

defEQ :: Name -> TH.Dec
defEQ (cons' -> t) = typeInstance2 ''Cmp t t eq

-- | Generate an instance of the `Cmp` family to impose the following ordering:
--   > type instance Cmp tlt tgt = LT
--   > type instance Cmp tgt tlt = GT
--
--   which is equivalent to saying: tlt < tgt
defOrderedPair :: Name -> Name -> [TH.Dec]
defOrderedPair tlt tgt = [defLT tlt tgt, defGT tlt tgt]

-- | Define a total ordering on the list of types.
--   [T1, T2, ..., Tn] => T1 < T2 < ... < Tn
defOrder :: [Name] -> Q [TH.Dec]
defOrder ns = pure $ ltgts <> eqs
    where ltgts = concatMap (uncurry defOrderedPair) $ pairs ns
          eqs   = defEQ <$> ns

