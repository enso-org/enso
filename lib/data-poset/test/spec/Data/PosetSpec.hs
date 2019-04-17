module Data.PosetSpec where

import Prologue

import qualified Data.Poset as Poset

import Test.Hspec              (Spec, describe, it)
import Test.Hspec.Expectations (shouldBe)

fromRels :: Ord n => [(Ordering, n, n)] -> Poset.Poset n
fromRels = foldl' (flip $ uncurry Poset.insertRelation) def

spec :: Spec
spec = describe "Relations Graph" $ do
    it "perserves transitivity upon equalzing" $ do
        let baseGraph = fromRels
                [(LT, 0, 1),
                 (LT, 2, 3)
                 ]
            graph = Poset.insertRelation EQ 1 2 baseGraph
        Poset.getRelation 0 3 graph `shouldBe` Just LT
    it "preserves transitivity upon adding inequalities" $ do
        let baseGraph = fromRels
                [(LT, 0, 1),
                 (LT, 2, 3)
                 ]
            graph = Poset.insertRelation GT 2 1 baseGraph
        Poset.getRelation 3 0 graph `shouldBe` Just GT
    it "does not add transitive relations in the wrong direction" $ do
        let baseGraph = fromRels
                [(LT, 0, 1),
                 (LT, 2, 3)
                 ]
            graph = Poset.insertRelation GT 1 2 baseGraph
        Poset.getRelation 3 0 graph `shouldBe` Nothing
    it "preserves equality transitivity" $ do
        let baseGraph = fromRels
                [(EQ, 0, 1),
                 (EQ, 2, 3)
                 ]
            graph = Poset.insertRelation EQ 1 2 baseGraph
        Poset.getRelation 3 0 graph `shouldBe` Just EQ
    it "preserves equality reflexivity" $ do
        let baseGraph = fromRels
                [(EQ, 0, 1),
                 (LT, 2, 3),
                 (EQ, 3, 4)
                 ]
        for_ [1, 2, 3, 4, 5] $ \i ->
            Poset.getRelation i i baseGraph `shouldBe` Just EQ
    it "perserves all equality laws inside one abstraction class" $ do
        let baseGraph = fromRels
                [(EQ, 0, 1),
                 (EQ, 1, 2),
                 (EQ, 2, 3)
                ]
            allNodes = [0, 1, 2, 3]
            allPairs = (,) <$> allNodes <*> allNodes
        for_ allPairs $ \(i, j) ->
            Poset.getRelation i j baseGraph `shouldBe` Just EQ

    it "does not break upon equalizing an element with itself" $ do
        let baseGraph = fromRels [(LT, 0, 1)]
            graph = Poset.insertRelation EQ 0 0 baseGraph
        Poset.getRelation 0 1 graph `shouldBe` Just LT

