module Language.Symbol.Operator.Prec.Poset where

import Prologue

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Lens (non, (?~))
import Data.Map     (Map)
import Data.Set     (Set)

-----------------------
-- === Partition === --
-----------------------

-- === Definition === --

data Partition n = Partition
    { _representantsMap :: Map n n
    , _membersMap       :: Map n (Set n)
    } deriving (Show, Eq)
makeLenses ''Partition

instance Default (Partition n) where
    def = Partition def def

-- === Private API === --

setRepresentant :: Ord n => n -> n -> Partition n -> Partition n
setRepresentant rep item partition = let
    fixedRepMap     = partition & representantsMap . at item ?~ rep
    fixedMembership
        = fixedRepMap & membersMap . at rep . non def %~ Set.insert item
    in fixedMembership

-- === Public API === --

equalize :: Ord n => n -> n -> Partition n -> Partition n
equalize tgt src partition = let
    srcMembers = partition ^. membersMap . at src . non def
    fixedReps
        = foldl' (flip $ setRepresentant tgt) partition $ Set.elems srcMembers
    removedSrc = fixedReps & membersMap . at src .~ Nothing
    fixedSrcRepresentant = setRepresentant tgt src removedSrc
    in fixedSrcRepresentant

----------------------------
-- === RelationsGraph === --
----------------------------

-- === Definition === --

data Node n = Node
    { _lesser  :: Set n
    , _greater :: Set n
    } deriving (Show, Eq)
makeLenses ''Node

instance Default (Node n) where
    def = Node def def

type RelationsGraph n = Map n (Node n)

-- === Private API === --

deleteNode :: Ord n => n -> RelationsGraph n -> RelationsGraph n
deleteNode labelToRemove relations = let
    node = relations ^. at labelToRemove . non def
    removeRelation bucket nodesMap label
        = nodesMap & ix label . bucket %~ Set.delete labelToRemove
    fixedGt
        = foldl' (removeRelation greater) relations . Set.elems $ node ^. lesser
    fixedLt
        = foldl' (removeRelation lesser) fixedGt . Set.elems $ node ^. greater
    in fixedLt & at labelToRemove .~ Nothing

addRelations :: Ord n => Set n -> Set n -> RelationsGraph n -> RelationsGraph n
addRelations greaters lessers relations = let
    addRels items bucket nodesMap label
        = nodesMap & at label . non def . bucket %~ Set.union items
    fixedGt = foldl' (addRels greaters greater) relations $ Set.elems lessers
    fixedLt = foldl' (addRels lessers  lesser)  fixedGt   $ Set.elems greaters
    in fixedLt

-- === Public API === --

unify :: Ord n => n -> n -> RelationsGraph n -> RelationsGraph n
unify tgtLabel srcLabel relations = let
    tgtNode    = relations ^. at tgtLabel . non def
    srcNode    = relations ^. at srcLabel . non def
    withoutSrc = deleteNode srcLabel relations
    deleteSrc  = Set.delete srcLabel
    addTgt     = Set.insert tgtLabel
    commonGt   = addTgt . deleteSrc $
        Set.union (tgtNode ^. greater) (srcNode ^. greater)
    commonLt   = addTgt . deleteSrc $
        Set.union (tgtNode ^. lesser)  (srcNode ^. lesser)
    newNode    = Node commonLt commonGt
    mergedTgt  = withoutSrc & at tgtLabel ?~ newNode
    in addRelations commonGt commonLt mergedTgt

addInequality :: Ord n => n -> n -> RelationsGraph n -> RelationsGraph n
addInequality labelLt labelGt relations = let
    nodeLt = relations ^. at labelLt . non def
    nodeGt = relations ^. at labelGt . non def
    greaterThanGt = Set.insert labelGt $ nodeGt ^. greater
    lesserThanLt  = Set.insert labelLt $ nodeLt ^. lesser
    in addRelations greaterThanGt lesserThanLt relations

-------------------
-- === Poset === --
-------------------

-- === Definition === --

data Poset n = Poset
    { _relations :: RelationsGraph n
    , _partition :: Partition n
    } deriving (Show, Eq)
makeLenses ''Poset

instance Default (Poset n) where
    def = Poset def def

instance Mempty (Poset n) where
    mempty = def

-- === Private API === --

getNode :: Ord n => n -> Poset n -> Node n
getNode label graph = let
    representant = getRepresentant label graph
    in graph ^. relations . at representant . non def

getRepresentant :: Ord n => n -> Poset n -> n
getRepresentant item = view $ partition . representantsMap . at item . non item

merge :: Ord n => n -> n -> Poset n -> Poset n
merge tgtLabel srcLabel graph = let
    tgtNode = getNode tgtLabel graph
    srcNode = getNode srcLabel graph
    tgtRep  = getRepresentant tgtLabel graph
    srcRep  = getRepresentant srcLabel graph
    in if tgtRep == srcRep
        then graph
        else graph & relations %~ unify tgtRep srcRep
                   & partition %~ equalize tgtRep srcRep

addEquality :: Ord n => n -> n -> Poset n -> Poset n
addEquality label1 label2 graph = let
    in merge label1 label2 graph

addLt :: Ord n => n -> n -> Poset n -> Poset n
addLt lt gt graph = let
    repLt = getRepresentant lt graph
    repGt = getRepresentant gt graph
    in graph & relations %~ addInequality repLt repGt

-- === Public API === --

insertRelation :: Ord n => Ordering -> n -> n -> Poset n -> Poset n
insertRelation = \case
    LT -> addLt
    GT -> flip addLt
    EQ -> addEquality

getRelation :: Ord n => n -> n -> Poset n -> Maybe Ordering
getRelation label1 label2 graph = let
    rep1 = getRepresentant label1 graph
    rep2 = getRepresentant label2 graph
    node1 = getNode label1 graph
    isEq = if rep1 == rep2 then Just EQ else Nothing
    isLt = if Set.member rep2 (node1 ^. greater) then Just LT else Nothing
    isGt = if Set.member rep2 (node1 ^. lesser)  then Just GT else Nothing
    in isEq <|> isLt <|> isGt
