{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Language.Symbol.Operator.Prec.RelMap where

import Prologue

import           Control.Lens.Utils.Nested (at')
import           Data.Map                  (Map)
import Data.Set (Set)
import Control.Lens (non, (?~))
import qualified Data.Map                  as Map
import qualified Data.Set as Set
import Debug.Trace

data Node n = Node
    { _lesser  :: Set n
    , _greater :: Set n
    } deriving (Show, Eq)
makeLenses ''Node

instance Default (Node n) where
    def = Node def def

data Partition n = Partition
    { _representantsMap :: Map n n
    , _membersMap       :: Map n (Set n)
    } deriving (Show, Eq)
makeLenses ''Partition

instance Default (Partition n) where
    def = Partition def def

data RelGraph n = RelGraph
    { _nodes     :: Map n (Node n)
    , _partition :: Partition n
    } deriving (Show, Eq)
makeLenses ''RelGraph

instance Default (RelGraph n) where
    def = RelGraph def def

instance Mempty (RelGraph n) where
    mempty = def

getNode :: Ord n => n -> RelGraph n -> Node n
getNode label graph = let
    representant = getRepresentant label graph
    in graph ^. nodes . at representant . non def

getRepresentant :: Ord n => n -> RelGraph n -> n
getRepresentant item = view $ partition . representantsMap . at item . non item

mergeNodes :: Ord n => n -> Node n -> n -> Node n -> Map n (Node n) -> Map n (Node n)
mergeNodes tgtLabel tgtNode srcLabel srcNode nodesMap = let
    relabelAt bucket nodes label = nodes & ix label . bucket %~ (Set.insert tgtLabel . Set.delete srcLabel)
    relabeledLesser = foldl' (relabelAt lesser) nodesMap $ Set.elems $ srcNode ^. greater
    relabeledGreater = foldl' (relabelAt greater) relabeledLesser $ Set.elems $ srcNode ^. lesser
    deletedSrc = relabeledGreater & at srcLabel .~ Nothing
    newNode    = Node
        (Set.union (tgtNode ^. lesser)  (srcNode ^. lesser))
        (Set.union (tgtNode ^. greater) (srcNode ^. greater))
    mergedTgt  = deletedSrc & at tgtLabel ?~ newNode
    in mergedTgt

mergeEquivalenceClasses :: Ord n => n -> n -> Partition n -> Partition n
mergeEquivalenceClasses tgt src partition = let
    srcMembers = partition ^. membersMap . at src . non def
    tgtMembers = partition ^. membersMap . at tgt . non def
    fixedRepresentants = partition & representantsMap %~ flip (foldl' (\reps item -> reps & at item ?~ tgt)) (toList srcMembers)
    removedSrc = fixedRepresentants & membersMap . at src .~ Nothing
    fixedSrcRepresentant = removedSrc & representantsMap . at src ?~ tgt
    fixedTgtMembership = fixedSrcRepresentant & membersMap . at tgt ?~ (Set.insert src $ Set.union srcMembers tgtMembers)
    in fixedTgtMembership

merge :: Ord n => n -> n -> RelGraph n -> RelGraph n
merge tgtLabel srcLabel graph = let
    tgtNode = getNode tgtLabel graph
    srcNode = getNode srcLabel graph
    tgtRep  = getRepresentant tgtLabel graph
    srcRep  = getRepresentant srcLabel graph
    in if tgtRep == srcRep
        then graph
        else graph & nodes %~ mergeNodes tgtRep tgtNode srcRep srcNode
                   & partition %~ mergeEquivalenceClasses tgtRep srcRep

addEquality :: Ord n => n -> n -> RelGraph n -> RelGraph n
addEquality label1 label2 graph = let
    in merge label1 label2 graph

addLt :: Ord n => n -> n -> RelGraph n -> RelGraph n
addLt lt gt graph = let
    repLt = getRepresentant lt graph
    repGt = getRepresentant gt graph
    nodeLt = getNode lt graph
    nodeGt = getNode gt graph
    greaterThanGt = nodeGt ^. greater
    lesserThanLt = nodeLt ^. lesser
    in graph & nodes . at repLt . non def . greater %~ Set.insert repGt . Set.union greaterThanGt
             & nodes . at repGt . non def . lesser  %~ Set.insert repLt . Set.union lesserThanLt

insertRel :: (Show n, Ord n) => Ordering -> n -> n -> RelGraph n -> RelGraph n
insertRel ord n1 n2 g = traceShowId $ case ord of
    LT -> addLt n1 n2 g
    GT -> addLt n2 n1 g
    EQ -> addEquality n1 n2 g

getRelation :: Ord n => n -> n -> RelGraph n -> Maybe Ordering
getRelation label1 label2 graph = let
    rep1 = getRepresentant label1 graph
    rep2 = getRepresentant label2 graph
    node1 = getNode label1 graph
    isEq = if rep1 == rep2 then Just EQ else Nothing
    isLt = if Set.member rep2 (node1 ^. greater) then Just LT else Nothing
    isGt = if Set.member rep2 (node1 ^. lesser)  then Just GT else Nothing
    in isEq <|> isLt <|> isGt

----------------------------------------
---- === Precedence relations map === --
----------------------------------------
--
---- === Definition === --
--
--type RelMapping n = Map n (Map n Ordering)
--data RelMap     n = RelMap { _provided  :: !(RelMapping n)
--                            , _inferred :: !(RelMapping n)
--                            } deriving (Show)
--makeLenses ''RelMap
--
--
---- === Utils === --
--
--addUniRelTo, addBiRelTo :: Ord n => Lens' (RelMap n) (RelMapping n) -> Ordering -> n -> n -> RelMap n -> RelMap n
--addUniRelTo lens ord a b = lens %~ at a . at' b .~ Just ord
--addBiRelTo  lens ord a b = addUniRelTo lens ord a b . addUniRelTo lens (counterOrdering ord) b a
--
--addProvidedRel, addInferredRel :: Ord n => Ordering -> n -> n -> RelMap n -> RelMap n
--addProvidedRel = addBiRelTo provided
--addInferredRel = addBiRelTo inferred
--
---- Legend:
----     o - single operator connected with others
----     left / right connection - (<) precedence relation
----     top  / bottom           - (=) precedence relation
----
---- Input:
----         o
---- o-o-o-o-o
----           o-o-o-o-o-o
----           o
---- ^lefts^     ^rights^
----    lmids^
----           ^rmids
--insertRelEQ :: Ord n => n -> n -> RelMap n -> RelMap n
--insertRelEQ a b = inferAll . addProvidedRel EQ a b . addProvidedRel EQ b a
--    . addUniRelTo inferred EQ a a . addUniRelTo inferred EQ b b where
--    inferBorders m = foldl' (flip . uncurry $ addInferredRel LT) m
--        $ (,) <$> lefts m <*> rights m
--    inferLMids   m = foldl' (flip . uncurry $ addInferredRel LT) m
--        $ (,) <$> lmids m <*> rights m
--    inferRMids   m = foldl' (flip . uncurry $ addInferredRel LT) m
--        $ (,) <$> lefts m <*> rmids  m
--    inferMids    m = foldl' (flip . uncurry $ addInferredRel EQ) m
--        $ (,) <$> lmids m <*> rmids  m
--    inferAll       = inferMids . inferRMids . inferLMids . inferBorders
--    elems p t m = fmap fst . filter (p . snd) . Map.assocs . fromJust mempty
--        $ m ^. inferred . at t
--    lefts       = elems (== GT) a
--    rights      = elems (== LT) b
--    lmids       = elems (== EQ) a
--    rmids       = elems (== EQ) b
--
--insertRelLT :: Ord n => n -> n -> RelMap n -> RelMap n
--insertRelLT a b = inferAll . addProvidedRel LT a b . addUniRelTo inferred EQ a a . addUniRelTo inferred EQ b b where
--    inferAll  m = foldl' (flip . uncurry $ addInferredRel LT) m $ (,) <$> lefts m <*> rights m
--    elems p t m = fmap fst . filter (p . snd) . Map.assocs . fromJust mempty $ m ^. inferred . at t
--    lefts       = elems (/= LT) a
--    rights      = elems (/= GT) b
--
--insertRel :: Ord n => Ordering -> n -> n -> RelMap n -> RelMap n
--insertRel = \case
--    LT -> insertRelLT
--    GT -> flip insertRelLT
--    EQ -> insertRelEQ
--
--counterOrdering :: Ordering -> Ordering
--counterOrdering = \case LT -> GT
--                        EQ -> EQ
--                        GT -> LT
--
--
---- === Instances === --
--
--instance          Mempty    (RelMap n) where mempty = RelMap mempty mempty
--instance Ord n => Semigroup (RelMap n) where
--    m <> m' = RelMap (m ^. provided <> m' ^. provided)
--                        (m ^. inferred <> m' ^. inferred)
