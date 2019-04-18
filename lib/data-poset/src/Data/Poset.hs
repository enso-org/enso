module Data.Poset where

import Prologue

import qualified Data.Set as Set

import Control.Lens (non, (?~))
import Data.Map     (Map)
import Data.Set     (Set)



-----------------------
-- === Partition === --
-----------------------

-- === Definition === --

-- | `Partition n` is a way of representing a dynamic equivalence relation
--   over the domain `n`. Each element of `n` is tagged with a `representant`,
--   being a label for its equivalence class.
--   NB: This assumes a relation over the whole type `n`, any elements not
--   mentioned before are implicitly present in a singleton equivalence class.
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
    fixedRepMap = partition & representantsMap . at item ?~ rep
    fixedMembership =
        fixedRepMap & membersMap . at rep . non def %~ Set.insert item
    in fixedMembership


-- === Public API === --

-- | Add a new pair to the relation. It is simply performed by merging the
--   elements equivalence classes.
equalize :: Ord n => n -> n -> Partition n -> Partition n
equalize tgt src partition = let
    srcMembers = partition ^. membersMap . at src . non def
    fixedReps =
        foldl' (flip $ setRepresentant tgt) partition $ Set.elems srcMembers
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

-- | Represents a dynamic, antisymmetric and transitive relation (like `<`).
--   It is represented as a graph, in which each element stores all
--   the elements greater and smaller than itself.
type RelationsGraph n = Map n (Node n)


-- === Private API === --

deleteNode :: Ord n => n -> RelationsGraph n -> RelationsGraph n
deleteNode labelToRemove relations = let
    node = relations ^. at labelToRemove . non def
    removeRelation bucket nodesMap label =
        nodesMap & ix label . bucket %~ Set.delete labelToRemove
    fixedGt =
        foldl' (removeRelation greater) relations . Set.elems $ node ^. lesser
    fixedLt =
        foldl' (removeRelation lesser) fixedGt . Set.elems $ node ^. greater
    in fixedLt & at labelToRemove .~ Nothing

addRelations :: Ord n => Set n -> Set n -> RelationsGraph n -> RelationsGraph n
addRelations greaters lessers relations = let
    addRels items bucket nodesMap label =
        nodesMap & at label . non def . bucket %~ Set.union items
    fixedGt = foldl' (addRels greaters greater) relations $ Set.elems lessers
    fixedLt = foldl' (addRels lessers  lesser)  fixedGt   $ Set.elems greaters
    in fixedLt


-- === Public API === --

-- | Unifies two elements of the graph. Removes the second one from the
--   graph and then merges both corresponding nodes under the first's label.
--   It then updates the neighboring nodes, ensuring antisymmetry
--   and transitivity of the resulting graph.
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

-- | Adds a `<` relation between selected nodes. Then updates the neighboring
--   nodes, ensuring antisymmetry and transitivity of the resulting graph.
--   NB: It does not check whether the resulting graph is cyclical.
addInequality :: Ord n => n -> n -> RelationsGraph n -> RelationsGraph n
addInequality labelLt labelGt relations = let
    nodeLt = relations ^. at labelLt . non def
    nodeGt = relations ^. at labelGt . non def
    greaterThanGt = Set.insert labelGt $ nodeGt ^. greater
    lesserThanLt  = Set.insert labelLt $ nodeLt ^. lesser
    in addRelations greaterThanGt lesserThanLt relations



-------------------------------------------
-- === Poset (Partially Ordered Set) === --
-------------------------------------------

-- === Definition === --

-- | Represents a transitive, antisymmetric relation (like `<`), with an
--   additional notion of elements equality.
--   It is represented as a pair of a `RelationsGraph` and `Partition`.
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

-- | Inserts a new relation pair (represented as an `Ordering`)
--   into the `Poset`.
insertRelation :: Ord n => Ordering -> n -> n -> Poset n -> Poset n
insertRelation = \case
    LT -> addLt
    GT -> flip addLt
    EQ -> addEquality

-- | Returns the relation between requested elements. `Just Ordering` result
--   means the `Poset` defines a relation between these elements, `Nothing`
--   means the relation is not defined.
getRelation :: Ord n => n -> n -> Poset n -> Maybe Ordering
getRelation label1 label2 graph = let
    rep1 = getRepresentant label1 graph
    rep2 = getRepresentant label2 graph
    node1 = getNode label1 graph
    isEq = if rep1 == rep2 then Just EQ else Nothing
    isLt = if Set.member rep2 (node1 ^. greater) then Just LT else Nothing
    isGt = if Set.member rep2 (node1 ^. lesser)  then Just GT else Nothing
    in isEq <|> isLt <|> isGt
