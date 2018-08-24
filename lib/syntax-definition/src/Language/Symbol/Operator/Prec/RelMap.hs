{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Language.Symbol.Operator.Prec.RelMap where

import Prologue

import           Control.Lens.Utils.Nested (at')
import           Data.Map                  (Map)
import qualified Data.Map                  as Map

--------------------------------------
-- === Precedence relations map === --
--------------------------------------

-- === Definition === --

type RelMapping n = Map n (Map n Ordering)
data RelMap     n = RelMap { _provided  :: !(RelMapping n)
                            , _inferred :: !(RelMapping n)
                            } deriving (Show)
makeLenses ''RelMap


-- === Utils === --

addUniRelTo, addBiRelTo :: Ord n => Lens' (RelMap n) (RelMapping n) -> Ordering -> n -> n -> RelMap n -> RelMap n
addUniRelTo lens ord a b = lens %~ at a . at' b .~ Just ord
addBiRelTo  lens ord a b = addUniRelTo lens ord a b . addUniRelTo lens (counterOrdering ord) b a

addProvidedRel, addInferredRel :: Ord n => Ordering -> n -> n -> RelMap n -> RelMap n
addProvidedRel = addBiRelTo provided
addInferredRel = addBiRelTo inferred

-- Legend:
--     o - single operator connected with others
--     left / right connection - (<) precedence relation
--     top  / bottom           - (=) precedence relation
--
-- Input:
--         o
-- o-o-o-o-o
--           o-o-o-o-o-o
--           o
-- ^lefts^     ^rights^
--    lmids^
--           ^rmids
insertRelEQ :: Ord n => n -> n -> RelMap n -> RelMap n
insertRelEQ a b = inferAll . addProvidedRel EQ a b . addUniRelTo inferred EQ a a . addUniRelTo inferred EQ b b where
    inferBorders m = foldl' (flip . uncurry $ addInferredRel LT) m $ (,) <$> lefts m <*> rights m
    inferLMids   m = foldl' (flip . uncurry $ addInferredRel LT) m $ (,) <$> lmids m <*> rights m
    inferRMids   m = foldl' (flip . uncurry $ addInferredRel LT) m $ (,) <$> lefts m <*> rmids  m
    inferMids    m = foldl' (flip . uncurry $ addInferredRel EQ) m $ (,) <$> lmids m <*> rmids  m
    inferAll       = inferMids . inferRMids . inferLMids . inferBorders
    elems p t m = fmap fst . filter (p . snd) . Map.assocs . fromMaybe mempty $ m ^. inferred . at t
    lefts       = elems (== GT) a
    rights      = elems (== LT) b
    lmids       = elems (== EQ) a
    rmids       = elems (== EQ) b

insertRelLT :: Ord n => n -> n -> RelMap n -> RelMap n
insertRelLT a b = inferAll . addProvidedRel LT a b . addUniRelTo inferred EQ a a . addUniRelTo inferred EQ b b where
    inferAll  m = foldl' (flip . uncurry $ addInferredRel LT) m $ (,) <$> lefts m <*> rights m
    elems p t m = fmap fst . filter (p . snd) . Map.assocs . fromMaybe mempty $ m ^. inferred . at t
    lefts       = elems (/= LT) a
    rights      = elems (/= GT) b

insertRel :: Ord n => Ordering -> n -> n -> RelMap n -> RelMap n
insertRel = \case
    LT -> insertRelLT
    GT -> flip insertRelLT
    EQ -> insertRelEQ

counterOrdering :: Ordering -> Ordering
counterOrdering = \case LT -> GT
                        EQ -> EQ
                        GT -> LT


-- === Instances === --

instance          Mempty    (RelMap n) where mempty = RelMap mempty mempty
instance Ord n => Semigroup (RelMap n) where
    m <> m' = RelMap (m ^. provided <> m' ^. provided)
                        (m ^. inferred <> m' ^. inferred)
