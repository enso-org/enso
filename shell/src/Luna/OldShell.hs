
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLists      #-}

module Luna.Shell where

-- import Prologue hiding (lookup, (!!), fromList, Empty)

-- data Interval t = Interval { _start :: !t
--                            , _end   :: !t
--                            } deriving (Show, Functor, Traversable, Foldable)
--
-- data Span t a = Span { _len :: t
--                      , _val :: a
--                      , _sub :: SpanTree t a
--                      } deriving (Show, Functor, Traversable, Foldable)
--
-- -- newtype Span t = Span t deriving (Show, Functor, Traversable, Foldable)
--
-- newtype SpanTree t a = SpanTree [Span t a] deriving (Show, Functor, Traversable, Foldable, Monoid)
--
--
-- -- fromIntervals :: [Section (Interval t) (SpanTree t a)] -> SpanTree t a
-- -- fromIntervals [] = mempty
-- -- fromIntervals (i : is) =
--
-- -- addSpan :: Section (Span t) (SpanTree t a )
--
-- main :: IO ()
-- main = do
--     let t = mempty :: SpanTree Int Int
--     print t
--     print "hello"




import qualified Data.ByteString.Lazy.Char8 as ByteString

import Luna.Prelude as Prelude hiding (String, cons, elem)
import qualified Luna.Prelude as Prelude
import Data.Aeson (encode)

import           Luna.IR
import qualified OCI.IR.Repr.Vis as Vis
import           OCI.IR.Repr.Vis (MonadVis)
import           OCI.Pass        (Pass, SubPass, Preserves, Events, Inputs, Outputs)
import qualified OCI.Pass        as Pass
import           OCI.IR.Layout.Typed (type (>>), type (:>))
import qualified OCI.IR.Layout.Typed as Layout
import qualified Data.TreeMap as TM
import Web.Browser (openBrowser )

import OCI.IR.Term
import qualified Data.Set as Set
import Data.Set (Set)

-- import Control.Monad.State (MonadState, StateT, execStateT, get, put)
-- import qualified Control.Monad.State as State
import Control.Monad.State.Dependent

import Data.RTuple (Assoc ((:=)))
import OCI.Pass.Manager as PM

import Data.Event as Event

import Data.Reflection

import qualified Data.ManagedVectorMap as Store
import Data.ManagedVectorMap (STRefM)
import Luna.IR.Layer.UID (ID)

import qualified Luna.IR.Expr as Term


import qualified Data.RTuple.Class as RT
import Data.Property

import System.Log
import System.Log.Logger.Format (nestedColorFormatter)

import GHC.Stack

import Data.TList (TList)
import qualified Data.TList as TList

import Control.Concurrent
import System.Exit
import qualified Data.Graph.Class as Graph

import Control.Monad.Raise

import qualified Luna.Syntax.Text.Parser.Parsing as Parsing
import qualified Luna.Syntax.Text.Layer.Loc as Loc
import Data.TypeDesc
import qualified Luna.Syntax.Text.Parser.Parser   as Parser
import           Luna.Syntax.Text.Parser.Parser   (Parser, ParsedExpr, ReparsingStatus)
import           Luna.Syntax.Text.Parser.Class    (Parsing)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Marker (MarkedExprMap)
-- import Luna.Pass.Transform.Parsing (testSpec)
import Data.SpanTree
import Data.Text.Position
import Luna.Syntax.Text.Source
import Data.String.Utils

import qualified Data.Map as Map
import           Data.Map (Map)
import Luna.IR.Term.World (World, initWorld)

import qualified Luna.Pass.Sourcing.UnitLoader as UL
import Luna.IR.Term.Unit (UnitSet)
import qualified Data.TreeSet as TreeSet
-- import qualified Luna.Syntax.Text.Parser.Loader as ParserLoader

data ShellTest
type instance Abstract ShellTest = ShellTest
type instance Inputs  Net   ShellTest = '[AnyExpr]
type instance Outputs Net   ShellTest = '[AnyExpr]
type instance Inputs  Layer ShellTest = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs]
type instance Outputs Layer ShellTest = '[]
type instance Inputs  Attr  ShellTest = '[ReparsingStatus, WorldExpr]
type instance Outputs Attr  ShellTest = '[]
type instance Inputs  Event ShellTest = '[] -- will never be used
type instance Outputs Event ShellTest = '[New // AnyExpr]
type instance Preserves     ShellTest = '[]


-- pass1 :: (MonadFix m, MonadIO m, MonadIR m, MonadVis m, MonadPassManager m) => Pass ShellTest m
-- pass1 = gen_pass1

test_pass1 :: (MonadIO m, MonadFix m, PrimMonad m, MonadVis m, Logging m, Throws '[RefLookupError, IRError, PassEvalError] m) => m ()
test_pass1 = evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
    return ()
    runRegs' False

    Loc.init
    attachLayer 5 (getTypeDesc @Range) (getTypeDesc @AnyExpr)

    Parser.init
    attachLayer 5 (getTypeDesc @Parser) (getTypeDesc @AnyExpr)

    CodeSpan.init
    attachLayer 5 (getTypeDesc @CodeSpan) (getTypeDesc @AnyExpr)

    -- ParserLoader.init

    setAttr (getTypeDesc @SourceTree)    $ (mempty :: SourceTree)
    setAttr (getTypeDesc @MarkedExprMap) $ (mempty :: MarkedExprMap)
    setAttr (getTypeDesc @ParsedExpr)    $ (error "Data not provided: ParsedExpr")

    -- setAttr (getTypeDesc @Source) $ ("‹0›11" :: Source)
    -- Pass.eval' (Parsing.parserPass Parsing.expr)
    --
    setAttr (getTypeDesc @ReparsingStatus) $ (mempty :: ReparsingStatus)
    -- setAttr (getTypeDesc @Source) $ ("‹0›def foo x:\n ‹1›x+2\n ‹3›A+2" :: Source)
    -- Pass.eval' Parsing.unitReparser

    -- World initialization
    setAttr (getTypeDesc @WorldExpr) (undefined :: WorldExpr) -- FIXME[WD]: it's ugly, we have to find a way to initialize attrs by pass manager
    Pass.eval' initWorld

    -- Unit initialization
    -- setAttr (getTypeDesc @UnitSet) ( [ ("MyLib", [ ["Main"] ])
    --                                  , ("Std"  , [ ["Math", "Vector"]
    --                                              , ["A", "B", "C"]
    --                                              ]
    --                                    )
    --                                  ] :: UnitSet)
    setAttr (getTypeDesc @UnitSet) ( [ ("MyLib", [ ["Main"] ])
                                     , ("Std"  , [ ["A"]    ])
                                     ] :: UnitSet)
    Pass.eval' @UL.UnitInitializer UL.runUnitInitializer

    -- Preparing units to load
    setAttr (getTypeDesc @UL.UnitsToLoad) (mempty :: UL.UnitsToLoad)
    setAttr (getTypeDesc @UL.UnitsToLoadRequest) (wrap [["MyLib", "Main"]] :: UL.UnitsToLoadRequest)
    Pass.eval' @UL.UnitRequester UL.unitRequester

    -- Unit loading
    setAttr (getTypeDesc @UL.SourcesManager) (UL.fsSourceManager $ Map.insert ["MyLib", "Main"] (UL.Source "/home/wdanilo/dev/tmp/Main.luna" "/home/wdanilo/dev/tmp/Main.lunai")
                                                                 $ Map.insert ["Std"  , "A"   ] (UL.Source "/home/wdanilo/dev/tmp/A.luna"    "/home/wdanilo/dev/tmp/A.lunai")
                                                                 $ mempty
                                             )
    Pass.eval' @UL.UnitLoader      UL.unitLoader
    Pass.eval' @UL.ImportsResolver UL.importsResolver


    Pass.eval' @ShellTest $ do
        putStrLn "\nReparsingStatus:"
        pprint =<< getAttr @ReparsingStatus
        Vis.snapshotNoType "after parsing"
-- newtype UnitLoadReqs = UnitLoadReqs [Expr UnitReq]          deriving (Show)

-- newtype SrcPathMap   = SrcPathMap   (Map QualName FilePath) deriving (Show)


uncheckedDeleteStar :: (MonadRef m, Reader Layer (AnyExpr // Type) m, Editors Net '[Link' AnyExpr, AnyExpr] m) => Expr l -> m ()
uncheckedDeleteStar e = do
    freeElem =<< getLayer @Type e
    freeElem e
{-# INLINE uncheckedDeleteStar #-}

uncheckedDeleteStarType :: (MonadRef m, Reader Layer (AnyExpr // Type) m, Editors Net '[Link' AnyExpr, AnyExpr] m, Editors Layer '[Link' AnyExpr // Model] m)
                        => Expr l -> m ()
uncheckedDeleteStarType e = do
    typeLink     <- getLayer @Type e
    (oldStar, _) <- getLayer @Model typeLink
    uncheckedDeleteStar oldStar
    freeElem typeLink
{-# INLINE uncheckedDeleteStarType #-}







gen_pass1 :: ( MonadIO m, MonadRef m
             , Writers Net '[AnyExpr] m
             , Emitter (New // AnyExpr) m
             , Readers Layer '[AnyExpr // Model, AnyExpr // Succs] m
             , Vis.Snapshot m
            --  , Accessibles m '[AnyExpr // Model, Link' AnyExpr // Model, AnyExpr // Type, AnyExpr // Succs, Link' AnyExpr // UID, AnyExpr // UID, ExprNet, ExprLinkNet, ExprGroupNet]
            --  , Emitter m (New // AnyExpr)
             ) => m ()
gen_pass1 = do
    (s :: Expr Star) <- star
    Vis.snapshot "s1"
    (s :: Expr Star) <- star
    Vis.snapshot "s2"
    (s :: Expr Star) <- star
    -- ss <- string "hello"
    -- (s :: Expr Star) <- star
    tlink   <- getLayer @Type s
    (src,_) <- getLayer @Model tlink
    scss    <- getLayer @Succs src
    print src
    print scss
    --
    -- i <- getLayer @UID s
    -- print i
    --
    Vis.snapshot "s3"
    --
    --
    matchExpr s $ \case
        Unify l r -> print "ppp"
        Star      -> matchExpr s $ \case
            Unify l r -> print "hola"
            Star      -> print "hellox"


    return ()


--
-- testNodeRemovalCoherence :: IO (Either Pass.InternalError [Incoherence])
-- testNodeRemovalCoherence = runGraph $ do
--     foo   <- string "foo"
--     bar   <- string "bar"
--     vfoo  <- var foo
--     vbar  <- var bar
--     vbar' <- var bar0
--     uni   <- unify vfoo vbar
--     delete vbar'
--     delete uni
--     checkCoherence
--



main :: HasCallStack => IO ()
main = do



    -- runTaggedLogging $ runEchoLogger $ plain $ runFormatLogger nestedReportedFormatter $ do
    -- forkIO $ do
    runTaggedLogging $ runEchoLogger $ runFormatLogger nestedColorFormatter $ do
        (p, vis) <- Vis.newRunDiffT $ tryAll test_pass1
        case p of
            Left  e -> critical $ convert $ displayException e
            Right _ -> do
                let cfg = replace "#" "%23" $ ByteString.unpack $ encode $ vis
                -- putStrLn cfg
                liftIO $ openBrowser ("http://localhost:8000?cfg=" <> cfg)
                return ()
        -- print p
    -- putStrLn "\n------------\n"
    -- TM.main
    -- Graph.xmain
    -- testSpec

    -- threadDelay 1000
    -- die "die"

    -- lmain


------ Old Notes
----------------



-- (strName :: Expr String) <- rawString "String"
-- (strCons :: Expr (Cons #> String)) <- cons strName
-- Vis.snapshot "s1"
-- let strCons' = unsafeRelayout strCons :: Expr Layout.Cons'
--     strName' = unsafeRelayout strName :: Expr String'
-- newTypeLink <- link strCons' strName'
-- uncheckedDeleteStarType strName'
-- writeLayer @Type newTypeLink strName'
-- Vis.snapshot "s2"
--
-- let string s = do
--         foo <- rawString s
--         let foo' = unsafeRelayout foo :: Expr String'
--         ftlink <- link strCons' foo'
--         uncheckedDeleteStarType foo'
--         writeLayer @Type ftlink foo'
--         return foo'
--
-- s1 <- string "s1"
-- s2 <- string "s2"
-- s3 <- string "s3"
--
-- g <- group [s1,s2,s3]
-- print g
--
-- (v :: Expr $ Var #> String') <- var s1
--
-- let v' :: Expr Draft
--     v' = generalize v
--
-- -- (u :: Expr (Unify >> Phrase >> NT String' (Value >> ENT Int String' Star))) <- unify s2 v
-- (u :: Expr (Unify >> Phrase >> NT String' (Value >> ENT Star String' Star))) <- unify s2 v
--
-- (u' :: Expr (Unify >> Draft)) <- unify v' v'
--
-- print =<< checkCoherence



-- (a :: Expr Int Star)) <- var aName
-- b <- var "b"

-- (u :: Expr (ENT _ _ _)) <- unify a b
-- -- (f :: Expr (ENT Star Star Star)) <- acc "f" u
--
--
--
-- -- Vis.snapshot "s3"
-- d <- getLayer @Type u
-- print d
--
--
-- md <- getAttr @MyData
-- print md
--
-- ts <- exprs
-- print ts









-- type Size   = Int -- number of children
-- type Height = Int -- maximum distance to a leaf.
-- data Tree k v = Empty
--               | Tree !k !v !Size !Height !(Tree k v) !(Tree k v) deriving (Ord, Eq, Show)
--
-- instance Monoid (Tree k v) where
--     mempty = Empty
--
-- -- | /O(1)/. 'singleton' constructs a singleton AVL tree
-- singleton :: (Ord k) => k -> v -> Tree k v
-- singleton k v = Tree k v 1 1 Empty Empty
--
-- -- | /O(1)/. 'null' returns True if a tree is empty, otherwise False.
-- null :: Tree k v -> Bool
-- null Empty = True
-- null _    = False
--
-- -- | /O(1)/. 'head' returns the head of a tree.
-- head :: (Ord k) => Tree k v -> v
-- head Empty = error "took the head of an empty tree"
-- head (Tree _ v _ _ _ _) = v
--
-- -- | /O(lg n)/. 'tail' discards the head of the tree and returns a tree.
-- tail :: (Ord k) => Tree k v -> Tree k v
-- tail Empty = error "took the tail of an empty tree"
-- tail t@(Tree k _ _ _ _ _) = delete k t
--
-- -- | /O(1)/. 'size' reports the number of children in a tree
-- size :: Tree k v -> Int
-- size Empty = 0
-- size (Tree _ _ s _ _ _) = s
--
-- -- | /O(1)/. 'height' reports the maximum distance to a leaf.
-- height :: Tree k v -> Int
-- height Empty = 0
-- height (Tree _ _ _ h _ _) = h
--
-- findHeight :: Tree k v -> Tree k v -> Int
-- findHeight a b = 1 + max (height a) (height b)
--
-- findSize :: Tree k v -> Tree k v -> Int
-- findSize a b = 1 + size a + size b
--
-- balance :: (Ord k) => Tree k v -> Tree k v
-- balance Empty = Empty
-- balance t@(Tree k v _ _ l r)
--   | abs (height l - height r) < 2 = t
--   | height l < height r =
--     case r of
--       Empty                -> impossible
--       Tree rk rv _ _ rl rr -> Tree rk rv (findSize nl rr) (findHeight nl rr) nl rr
--                               where nl = Tree k v (findSize l rl) (findHeight l rl) l rl
--
--   | otherwise =
--     case l of
--       Empty                -> impossible
--       Tree lk lv _ _ ll lr -> Tree lk lv (findSize ll nr) (findHeight ll nr) ll nr
--                               where nr = (Tree k v (findSize lr r) (findHeight lr r) lr r)
--
-- (!!) :: (Ord k) => Tree k v -> Int -> (k,v)
-- (!!) Empty _ = error "index out of bounds"
-- (!!) (Tree k v d _ l r) n
--   | n > d = error "index out of bounds"
--   | otherwise =
--     let l' = size l in
--       if n == l'
--       then (k,v)
--       else if n <= l'
--            then l !! n
--            else r !! (n - l' - 1)
--
-- -- | /O(lg n)/.
-- lookup :: (Ord k) => k -> Tree k v -> Maybe v
-- lookup _ Empty = Nothing
-- lookup k' (Tree k v _ _ l r)
--   | k == k' = Just v
--   | k' < k = lookup k' l
--   | otherwise = lookup k' r
--
-- -- | /O(lg n)/.
-- insert :: (Ord k) => k -> v -> Tree k v -> Tree k v
-- insert k v Empty                = singleton k v
-- insert k v (Tree k1 v1 s _ l r) = balance $ if k <= k1 then let l' = insert k v l in Tree k1 v1 (s + 1) (findHeight l' r) l' r
--                                                        else let r' = insert k v r in Tree k1 v1 (s + 1) (findHeight l r') l r'
--
-- insert2 :: (Ord k) => (k,k) -> v -> Tree k v -> Tree k v
-- insert2 (0, len) v Empty                = singleton len v
-- insert2 k@(off, len') v (Tree len v1 s _ l r) = balance $ if (off + len') <= len
--     then Tree len v1 (s + 1) (findHeight l' r) l' r where l' = insert2 k v l
--     else Tree len v1 (s + 1) (findHeight l r') l r' where r' = insert2 k v r
--
-- -- | /O(lg n)/.
-- delete :: (Ord k) => k -> Tree k v -> Tree k v
-- delete _ Empty = Empty
-- delete k t@(Tree k1 _ _ _ Empty Empty) = if k == k1 then Empty else t
-- delete k t@(Tree k1 v1 _ _ l r)
--   | k == k1 =
--     case t of
--       Empty -> Empty
--       (Tree _ _ _ _ Empty r1) ->
--         case getLeft r1 of
--           (Nothing, _) -> Empty
--           (Just (k', v'), r') ->
--             balance (Tree k' v' (findSize Empty r') (findHeight Empty r') Empty r')
--       (Tree _ _ _ _ l1 r1) ->
--         case getRight l1 of
--           (Nothing, _) -> Empty
--           (Just (k', v'), l') ->
--             balance (Tree k' v' (findSize l' r1) (findHeight l' r1) l' r1)
--     | k < k1 =
--       let l' = delete k l in
--         balance (Tree k1 v1 (findSize l' r) (findHeight l' r) l' r)
--     | otherwise =
--       let r' = delete k r in
--         balance (Tree k1 v1 (findSize l r') (findHeight l r') l r')
--
-- getRight :: (Ord k) => Tree k v -> (Maybe (k,v), Tree k v)
-- getRight Empty = (Nothing, Empty)
-- getRight (Tree k v _ _ Empty Empty) = (Just (k,v), Empty)
-- getRight (Tree k v _ _ l Empty) = (Just (k,v), l)
-- getRight (Tree k v _ _ l r) =
--   case getRight r of
--     (p, t2) -> (p, balance (Tree k v (findSize l t2) (findHeight l t2) l t2))
--
-- getLeft :: (Ord k) => Tree k v -> (Maybe (k,v), Tree k v)
-- getLeft Empty = (Nothing, Empty)
-- getLeft (Tree k v _ _ Empty Empty) = (Just (k,v), Empty)
-- getLeft (Tree k v _ _ Empty r) = (Just (k,v), r)
-- getLeft (Tree k v _ _ _ r) =
--   case getLeft r of
--     (p, t2) -> (p, Tree k v (findSize r t2) (findHeight r t2) t2 r)
--
-- -- | /O(n lg n)/.
-- fromList :: (Ord k) => [(k,v)] -> Tree k v
-- fromList [] = Empty
-- fromList ((k,v):[]) = singleton k v
-- fromList ((k,v):tl) = insert k v (fromList tl)
--
-- -- | /O(n lg n)/.
-- fromAscList :: (Ord k) => [(k,v)] -> Tree k v
-- fromAscList = fromList
--
-- -- TODO implement an instance of foldable so that this can be concisely defined
-- -- | /O(n lg n)/.
-- toAscList :: (Ord k) => Tree k v -> [(k,v)]
-- toAscList Empty = []
-- toAscList (Tree k v _ _ l r) = toAscList l ++ (k,v) : toAscList r
--
-- -- | /O(n lg n)/.
-- toList :: (Ord k) => Tree k v -> [(k,v)]
-- toList = toAscList
--
--
-- main :: IO ()
-- main = do
--     let t = mempty :: Tree Int String
--         t' = insert 20 "b"
--            $ insert 10 "a"
--            $ t
--     print t'
--     print $ lookup 10 t'
--     return ()




--
--
-- module Data.FingerTree (
-- #if TESTING
--     FingerTree(..), Digit(..), Node(..), deep, node2, node3,
-- #else
--     FingerTree,
-- #endif
--     Measured(..),
--     -- * Construction
--     empty, singleton,
--     (<|), (|>), (><),
--     fromList,
--     -- * Deconstruction
--     null,
--     ViewL(..), ViewR(..), viewl, viewr,
--     split, takeUntil, dropUntil,
--     -- * Transformation
--     reverse,
--     fmap', fmapWithPos, unsafeFmap,
--     traverse', traverseWithPos, unsafeTraverse
--     -- * Example
--     -- $example
--     ) where












--
-- import Prologue hiding (Empty, null, reverse, fromList, (<|), (|>), deep, (:<), toList, (:>))
--
-- import Control.Applicative (Applicative(pure, (<*>)), (<$>))
-- import Data.Monoid
-- import Data.Foldable (Foldable(foldMap), toList)
--
--
-- data ViewL s a = EmptyL | a :< s a deriving (Eq, Ord, Show, Read, Functor)
-- data ViewR s a = EmptyR | s a :> a deriving (Eq, Ord, Show, Read, Functor)
--
--
-- instance Measured a => Monoid (FingerTree a) where
--     mempty  = Empty       ; {-# INLINE mempty  #-}
--     mappend = appendTree0 ; {-# INLINE mappend #-} -- | /O(log(min(n1,n2)))/
--
--
-- data Digit a = One   a
--              | Two   a a
--              | Three a a a
--              | Four  a a a a
--              deriving (Show, Functor, Foldable)
--
--
-- -------------------
-- -- 4.1 Measurements
-- -------------------
--
-- -- | Things that can be measured.
-- type family Measure a
-- class Monoid (Measure a) => Measured a where
--     measure :: a -> Measure a
--
-- instance Measured a => Measured (Digit a) where
--     measure = foldMap measure
--
-- type instance Measure (Digit a) = Measure a
--
-- ---------------------------
-- -- 4.2 Caching measurements
-- ---------------------------
--
-- data Node a = Node2 !(Measure a) a a | Node3 !(Measure a) a a a
--
-- deriving instance (Show a, Show (Measure a)) => Show (Node a)
--
-- type instance Measure (Node a) = Measure a
--
-- instance Foldable Node where
--     foldMap f (Node2 _ a b) = f a `mappend` f b
--     foldMap f (Node3 _ a b c) = f a `mappend` f b `mappend` f c
--
-- node2        ::  Measured a => a -> a -> Node a
-- node2 a b    =   Node2 (measure a `mappend` measure b) a b
--
-- node3        ::  Measured a => a -> a -> a -> Node a
-- node3 a b c  =   Node3 (measure a `mappend` measure b `mappend` measure c) a b c
--
-- instance Monoid (Measure a) => Measured (Node a) where
--     measure (Node2 v _ _)    =  v
--     measure (Node3 v _ _ _)  =  v
--
-- nodeToDigit :: Node a -> Digit a
-- nodeToDigit (Node2 _ a b) = Two a b
-- nodeToDigit (Node3 _ a b c) = Three a b c
--
-- -- | A representation of a sequence of values of type @a@, allowing
-- -- access to the ends in constant time, and append and split in time
-- -- logarithmic in the size of the smaller piece.
-- --
-- -- The collection is also parameterized by a measure type @v@, which
-- -- is used to specify a position in the sequence for the 'split' operation.
-- -- The types of the operations enforce the constraint @'Measured' v a@,
-- -- which also implies that the type @v@ is determined by @a@.
-- --
-- -- A variety of abstract data types can be implemented by using different
-- -- element types and measurements.
-- data FingerTree a
--     = Empty
--     | Single a
--     | Deep !(Measure a) !(Digit a) (FingerTree (Node a)) !(Digit a)
--
-- type instance Measure (FingerTree a) = Measure a
--
-- deriving instance (Show a, Show (Measure a)) => Show (FingerTree a)
--
-- deep :: Measured a => Digit a -> FingerTree (Node a) -> Digit a -> FingerTree a
-- deep pr m sf = Deep ((measure pr `mappendVal` m) `mappend` measure sf) pr m sf
--
-- -- | /O(1)/. The cached measure of a tree.
-- instance Measured a => Measured (FingerTree a) where
--     measure Empty           =  mempty
--     measure (Single x)      =  measure x
--     measure (Deep v _ _ _)  =  v
--
-- instance Foldable FingerTree where
--     foldMap _ Empty = mempty
--     foldMap f (Single x) = f x
--     foldMap f (Deep _ pr m sf) =
--         foldMap f pr `mappend` foldMap (foldMap f) m `mappend` foldMap f sf
--
-- instance Eq a => Eq (FingerTree a) where
--     xs == ys = toList xs == toList ys
--
-- instance Ord a => Ord (FingerTree a) where
--     compare xs ys = compare (toList xs) (toList ys)
--
-- -- instance Show a => Show (FingerTree a) where
-- --     showsPrec p xs = showParen (p > 10) $
-- --         showString "fromList " . shows (toList xs)
--
-- -- | Like 'fmap', but with a more constrained type.
-- fmap' :: Measured a2 => (a1 -> a2) -> FingerTree a1 -> FingerTree a2
-- fmap' = mapTree
--
-- mapTree :: (Measured a2) =>
--     (a1 -> a2) -> FingerTree a1 -> FingerTree a2
-- mapTree _ Empty = Empty
-- mapTree f (Single x) = Single (f x)
-- mapTree f (Deep _ pr m sf) =
--     deep (mapDigit f pr) (mapTree (mapNode f) m) (mapDigit f sf)
--
-- mapNode :: (Measured a2) =>
--     (a1 -> a2) -> Node a1 -> Node a2
-- mapNode f (Node2 _ a b) = node2 (f a) (f b)
-- mapNode f (Node3 _ a b c) = node3 (f a) (f b) (f c)
--
-- mapDigit :: (a -> b) -> Digit a -> Digit b
-- mapDigit f (One a) = One (f a)
-- mapDigit f (Two a b) = Two (f a) (f b)
-- mapDigit f (Three a b c) = Three (f a) (f b) (f c)
-- mapDigit f (Four a b c d) = Four (f a) (f b) (f c) (f d)
--
-- -- | Map all elements of the tree with a function that also takes the
-- -- measure of the prefix of the tree to the left of the element.
-- fmapWithPos :: (Measured a1, Measured a2) => (Measure a1 -> a1 -> a2) -> FingerTree a1 -> FingerTree a2
-- fmapWithPos f = mapWPTree f mempty
--
-- mapWPTree :: (Measured a1, Measured a2) => (Measure a1 -> a1 -> a2) -> Measure a1 -> FingerTree a1 -> FingerTree a2
-- mapWPTree _ _ Empty = Empty
-- mapWPTree f v (Single x) = Single (f v x)
-- mapWPTree f v (Deep _ pr m sf) =
--     deep (mapWPDigit f v pr)
--          (mapWPTree (mapWPNode f) vpr m)
--          (mapWPDigit f vm sf)
--   where
--     vpr     =  v    `mappend`  measure pr
--     vm      =  vpr  `mappendVal` m
--
-- mapWPNode :: (Measured a1, Measured a2) => (Measure a1 -> a1 -> a2) -> Measure a1 -> Node a1 -> Node a2
-- mapWPNode f v (Node2 _ a b) = node2 (f v a) (f va b)
--   where
--     va      = v `mappend` measure a
-- mapWPNode f v (Node3 _ a b c) = node3 (f v a) (f va b) (f vab c)
--   where
--     va      = v `mappend` measure a
--     vab     = va `mappend` measure b
--
-- mapWPDigit :: Measured a => (Measure a -> a -> b) -> Measure a -> Digit a -> Digit b
-- mapWPDigit f v (One a) = One (f v a)
-- mapWPDigit f v (Two a b) = Two (f v a) (f va b)
--   where va      = v `mappend` measure a
-- mapWPDigit f v (Three a b c) = Three (f v a) (f va b) (f vab c)
--   where va      = v `mappend` measure a
--         vab     = va `mappend` measure b
-- mapWPDigit f v (Four a b c d) = Four (f v a) (f va b) (f vab c) (f vabc d)
--   where va      = v `mappend` measure a
--         vab     = va `mappend` measure b
--         vabc    = vab `mappend` measure c
--
-- type EqMeasures a b = (Measure a ~ Measure b)
-- -- | Like 'fmap', but safe only if the function preserves the measure.
-- unsafeFmap :: EqMeasures a b => (a -> b) -> FingerTree a -> FingerTree b
-- unsafeFmap _ Empty = Empty
-- unsafeFmap f (Single x) = Single (f x)
-- unsafeFmap f (Deep v pr m sf) =
--     Deep v (mapDigit f pr) (unsafeFmap (unsafeFmapNode f) m) (mapDigit f sf)
--
-- unsafeFmapNode :: EqMeasures a b => (a -> b) -> Node a -> Node b
-- unsafeFmapNode f (Node2 v a b) = Node2 v (f a) (f b)
-- unsafeFmapNode f (Node3 v a b c) = Node3 v (f a) (f b) (f c)
--
-- -- | Like 'traverse', but with a more constrained type.
-- traverse' :: (Measured a2, Applicative f) =>
--     (a1 -> f a2) -> FingerTree a1 -> f (FingerTree a2)
-- traverse' = traverseTree
--
-- traverseTree :: (Measured a2, Applicative f) =>
--     (a1 -> f a2) -> FingerTree a1 -> f (FingerTree a2)
-- traverseTree _ Empty = pure Empty
-- traverseTree f (Single x) = Single <$> f x
-- traverseTree f (Deep _ pr m sf) =
--     deep <$> traverseDigit f pr <*> traverseTree (traverseNode f) m <*> traverseDigit f sf
--
-- traverseNode :: (Measured a2, Applicative f) =>
--     (a1 -> f a2) -> Node a1 -> f (Node a2)
-- traverseNode f (Node2 _ a b) = node2 <$> f a <*> f b
-- traverseNode f (Node3 _ a b c) = node3 <$> f a <*> f b <*> f c
--
-- traverseDigit :: (Applicative f) => (a -> f b) -> Digit a -> f (Digit b)
-- traverseDigit f (One a) = One <$> f a
-- traverseDigit f (Two a b) = Two <$> f a <*> f b
-- traverseDigit f (Three a b c) = Three <$> f a <*> f b <*> f c
-- traverseDigit f (Four a b c d) = Four <$> f a <*> f b <*> f c <*> f d
--
-- -- | Traverse the tree with a function that also takes the
-- -- measure of the prefix of the tree to the left of the element.
-- traverseWithPos :: (Measured a1, Measured a2, Applicative f) =>
--     (Measure a1 -> a1 -> f a2) -> FingerTree a1 -> f (FingerTree a2)
-- traverseWithPos f = traverseWPTree f mempty
--
-- traverseWPTree :: (Measured a1, Measured a2, Applicative f) =>
--     (Measure a1 -> a1 -> f a2) -> Measure a1 -> FingerTree a1 -> f (FingerTree a2)
-- traverseWPTree _ _ Empty = pure Empty
-- traverseWPTree f v (Single x) = Single <$> f v x
-- traverseWPTree f v (Deep _ pr m sf) =
--     deep <$> traverseWPDigit f v pr <*> traverseWPTree (traverseWPNode f) vpr m <*> traverseWPDigit f vm sf
--   where
--     vpr     =  v    `mappend`  measure pr
--     vm      =  vpr  `mappendVal` m
--
-- traverseWPNode :: (Measured a1, Measured a2, Applicative f) =>
--     (Measure a1 -> a1 -> f a2) -> Measure a1 -> Node a1 -> f (Node a2)
-- traverseWPNode f v (Node2 _ a b) = node2 <$> f v a <*> f va b
--   where
--     va      = v `mappend` measure a
-- traverseWPNode f v (Node3 _ a b c) = node3 <$> f v a <*> f va b <*> f vab c
--   where
--     va      = v `mappend` measure a
--     vab     = va `mappend` measure b
--
-- traverseWPDigit :: (Measured a, Applicative f) =>
--     (Measure a -> a -> f b) -> Measure a -> Digit a -> f (Digit b)
-- traverseWPDigit f v (One a) = One <$> f v a
-- traverseWPDigit f v (Two a b) = Two <$> f v a <*> f va b
--   where
--     va      = v `mappend` measure a
-- traverseWPDigit f v (Three a b c) = Three <$> f v a <*> f va b <*> f vab c
--   where
--     va      = v `mappend` measure a
--     vab     = va `mappend` measure b
-- traverseWPDigit f v (Four a b c d) = Four <$> f v a <*> f va b <*> f vab c <*> f vabc d
--   where
--     va      = v `mappend` measure a
--     vab     = va `mappend` measure b
--     vabc    = vab `mappend` measure c
--
-- -- | Like 'traverse', but safe only if the function preserves the measure.
-- unsafeTraverse :: (Applicative f, EqMeasures a b) =>
--     (a -> f b) -> FingerTree a -> f (FingerTree b)
-- unsafeTraverse _ Empty = pure Empty
-- unsafeTraverse f (Single x) = Single <$> f x
-- unsafeTraverse f (Deep v pr m sf) =
--     Deep v <$> traverseDigit f pr <*> unsafeTraverse (unsafeTraverseNode f) m <*> traverseDigit f sf
--
-- unsafeTraverseNode :: (Applicative f, EqMeasures a b) =>
--     (a -> f b) -> Node a -> f (Node b)
-- unsafeTraverseNode f (Node2 v a b) = Node2 v <$> f a <*> f b
-- unsafeTraverseNode f (Node3 v a b c) = Node3 v <$> f a <*> f b <*> f c
--
-- -----------------------------------------------------
-- -- 4.3 Construction, deconstruction and concatenation
-- -----------------------------------------------------
--
--
-- -- | /O(1)/. A singleton sequence.
-- singleton :: a -> FingerTree a
-- singleton = Single
--
-- -- | /O(n)/. Create a sequence from a finite list of elements.
-- fromList :: Measured a => [a] -> FingerTree a
-- fromList = foldr (<|) Empty
--
-- -- | /O(1)/. Add an element to the left end of a sequence.
-- -- Mnemonic: a triangle with the single element at the pointy end.
-- infixr 5 <|, :<
-- (<|) :: Measured a => a -> FingerTree a -> FingerTree a
-- a <| Empty                      =  Single a
-- a <| Single b                   =  deep (One a) Empty (One b)
-- a <| Deep v (Four b c d e) m sf = m `seq` Deep (measure a <> v) (Two a b) (node3 c d e <| m) sf
-- a <| Deep v pr m sf             = Deep (measure a <> v) (consDigit a pr) m sf
--
-- consDigit :: a -> Digit a -> Digit a
-- consDigit a (One b) = Two a b
-- consDigit a (Two b c) = Three a b c
-- consDigit a (Three b c d) = Four a b c d
-- consDigit _ (Four _ _ _ _) = illegal_argument "consDigit"
--
-- -- | /O(1)/. Add an element to the right end of a sequence.
-- -- Mnemonic: a triangle with the single element at the pointy end.
-- infixl 5 |>, :>
-- (|>) :: Measured a => FingerTree a -> a -> FingerTree a
-- Empty |> a              =  Single a
-- Single a |> b           =  deep (One a) Empty (One b)
-- Deep v pr m (Four a b c d) |> e = m `seq`
--     Deep (v `mappend` measure e) pr (m |> node3 a b c) (Two d e)
-- Deep v pr m sf |> x     =
--     Deep (v `mappend` measure x) pr m (snocDigit sf x)
--
-- snocDigit :: Digit a -> a -> Digit a
-- snocDigit (One a) b = Two a b
-- snocDigit (Two a b) c = Three a b c
-- snocDigit (Three a b c) d = Four a b c d
-- snocDigit (Four _ _ _ _) _ = illegal_argument "snocDigit"
--
-- -- | /O(1)/. Is this the empty sequence?
-- null :: FingerTree a -> Bool
-- null = \case Empty -> True
--              _     -> False
-- {-# INLINE null #-}
--
-- -- | /O(1)/. Analyse the left end of a sequence.
-- viewl :: Measured a => FingerTree a -> ViewL FingerTree a
-- viewl Empty                     =  EmptyL
-- viewl (Single x)                =  x :< Empty
-- viewl (Deep _ (One x) m sf)     =  x :< rotL m sf
-- viewl (Deep _ pr m sf)          =  lheadDigit pr :< deep (ltailDigit pr) m sf
--
-- rotL :: Measured a => FingerTree (Node a) -> Digit a -> FingerTree a
-- rotL m sf      =   case viewl m of
--     EmptyL  ->  digitToTree sf
--     a :< m' ->  Deep (measure m `mappend` measure sf) (nodeToDigit a) m' sf
--
-- lheadDigit :: Digit a -> a
-- lheadDigit (One a) = a
-- lheadDigit (Two a _) = a
-- lheadDigit (Three a _ _) = a
-- lheadDigit (Four a _ _ _) = a
--
-- ltailDigit :: Digit a -> Digit a
-- ltailDigit (One _) = illegal_argument "ltailDigit"
-- ltailDigit (Two _ b) = One b
-- ltailDigit (Three _ b c) = Two b c
-- ltailDigit (Four _ b c d) = Three b c d
--
-- -- | /O(1)/. Analyse the right end of a sequence.
-- viewr :: Measured a => FingerTree a -> ViewR FingerTree a
-- viewr Empty                     =  EmptyR
-- viewr (Single x)                =  Empty :> x
-- viewr (Deep _ pr m (One x))     =  rotR pr m :> x
-- viewr (Deep _ pr m sf)          =  deep pr m (rtailDigit sf) :> rheadDigit sf
--
-- rotR :: Measured a => Digit a -> FingerTree (Node a) -> FingerTree a
-- rotR pr m = case viewr m of
--     EmptyR  ->  digitToTree pr
--     m' :> a ->  Deep (measure pr `mappendVal` m) pr m' (nodeToDigit a)
--
-- rheadDigit :: Digit a -> a
-- rheadDigit (One   a)       = a
-- rheadDigit (Two   _ b)     = b
-- rheadDigit (Three _ _ c)   = c
-- rheadDigit (Four  _ _ _ d) = d
--
-- rtailDigit :: Digit a -> Digit a
-- rtailDigit (One   _)       = illegal_argument "rtailDigit"
-- rtailDigit (Two   a _)     = One   a
-- rtailDigit (Three a b _)   = Two   a b
-- rtailDigit (Four  a b c _) = Three a b c
--
-- digitToTree :: Measured a => Digit a -> FingerTree a
-- digitToTree (One   a)       = Single a
-- digitToTree (Two   a b)     = deep (One a)   Empty (One b)
-- digitToTree (Three a b c)   = deep (Two a b) Empty (One c)
-- digitToTree (Four  a b c d) = deep (Two a b) Empty (Two c d)
--
-- ----------------
-- -- Concatenation
-- ----------------
--
-- appendTree0 :: Measured a => FingerTree a -> FingerTree a -> FingerTree a
-- appendTree0 f f' = case (f,f') of
--     (Empty   , xs      ) -> xs
--     (xs      , Empty   ) -> xs
--     (Single x, xs      ) -> x <| xs
--     (xs      , Single x) -> xs |> x
--     (Deep _ pr1 m1 sf1, Deep _ pr2 m2 sf2) -> deep pr1 (addDigits0 m1 sf1 pr2 m2) sf2
-- {-# INLINE appendTree0 #-}
--
-- appendTree1 :: Measured a => FingerTree a -> a -> FingerTree a -> FingerTree a
-- appendTree1 f a f' = case (f,f') of
--     (Empty   , xs      ) -> a <| xs
--     (xs      , Empty   ) -> xs |> a
--     (Single x, xs      ) -> x <| a <| xs
--     (xs      , Single x) -> xs |> a |> x
--     (Deep _ pr1 m1 sf1, Deep _ pr2 m2 sf2) -> deep pr1 (addDigits1 m1 sf1 a pr2 m2) sf2
--
-- appendTree2 :: Measured a => FingerTree a -> a -> a -> FingerTree a -> FingerTree a
-- appendTree2 f a b f' = case (f, f') of
--     (Empty   , xs      ) -> a <| b <| xs
--     (xs      , Empty   ) -> xs |> a |> b
--     (Single x, xs      ) -> x <| a <| b <| xs
--     (xs      , Single x) -> xs |> a |> b |> x
--     (Deep _ pr1 m1 sf1, Deep _ pr2 m2 sf2) -> deep pr1 (addDigits2 m1 sf1 a b pr2 m2) sf2
--
-- appendTree3 :: Measured a => FingerTree a -> a -> a -> a -> FingerTree a -> FingerTree a
-- appendTree3 f a b c f' = case (f, f') of
--     (Empty   , xs      ) -> a <| b <| c <| xs
--     (xs      , Empty   ) -> xs |> a |> b |> c
--     (Single x, xs      ) -> x <| a <| b <| c <| xs
--     (xs      , Single x) -> xs |> a |> b |> c |> x
--     (Deep _ pr1 m1 sf1, Deep _ pr2 m2 sf2) -> deep pr1 (addDigits3 m1 sf1 a b c pr2 m2) sf2
--
-- appendTree4 :: Measured a => FingerTree a -> a -> a -> a -> a -> FingerTree a -> FingerTree a
-- appendTree4 f a b c d f' = case (f, f') of
--     (Empty   , xs      ) -> a <| b <| c <| d <| xs
--     (xs      , Empty   ) -> xs |> a |> b |> c |> d
--     (Single x, xs      ) -> x <| a <| b <| c <| d <| xs
--     (xs      , Single x) -> xs |> a |> b |> c |> d |> x
--     (Deep _ pr1 m1 sf1, Deep _ pr2 m2 sf2) -> deep pr1 (addDigits4 m1 sf1 a b c d pr2 m2) sf2
--
-- addDigits0 :: Measured a => FingerTree (Node a) -> Digit a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
-- addDigits0 m1 (One a)        (One   b)       m2 = appendTree1 m1 (node2 a b) m2
-- addDigits0 m1 (One a)        (Two   b c)     m2 = appendTree1 m1 (node3 a b c) m2
-- addDigits0 m1 (One a)        (Three b c d)   m2 = appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits0 m1 (One a)        (Four  b c d e) m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits0 m1 (Two a b)      (One   c)       m2 = appendTree1 m1 (node3 a b c) m2
-- addDigits0 m1 (Two a b)      (Two   c d)     m2 = appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits0 m1 (Two a b)      (Three c d e)   m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits0 m1 (Two a b)      (Four  c d e f) m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits0 m1 (Three a b c)  (One   d)       m2 = appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits0 m1 (Three a b c)  (Two   d e)     m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits0 m1 (Three a b c)  (Three d e f)   m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits0 m1 (Three a b c)  (Four  d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits0 m1 (Four a b c d) (One   e)       m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits0 m1 (Four a b c d) (Two   e f)     m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits0 m1 (Four a b c d) (Three e f g)   m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits0 m1 (Four a b c d) (Four  e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
--
--
--
-- addDigits1 :: Measured a => FingerTree (Node a) -> Digit a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
-- addDigits1 m1 (One a)        b (One c)        m2 = appendTree1 m1 (node3 a b c) m2
-- addDigits1 m1 (One a)        b (Two c d)      m2 = appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits1 m1 (One a)        b (Three c d e)  m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits1 m1 (One a)        b (Four c d e f) m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits1 m1 (Two a b)      c (One d)        m2 = appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits1 m1 (Two a b)      c (Two d e)      m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits1 m1 (Two a b)      c (Three d e f)  m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits1 m1 (Two a b)      c (Four d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits1 m1 (Three a b c)  d (One e)        m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits1 m1 (Three a b c)  d (Two e f)      m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits1 m1 (Three a b c)  d (Three e f g)  m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits1 m1 (Three a b c)  d (Four e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits1 m1 (Four a b c d) e (One f)        m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits1 m1 (Four a b c d) e (Two f g)      m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits1 m1 (Four a b c d) e (Three f g h)  m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits1 m1 (Four a b c d) e (Four f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
--
--
--
-- addDigits2 :: Measured a => FingerTree (Node a) -> Digit a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
-- addDigits2 m1 (One a)        b c (One   d)       m2 = appendTree2 m1 (node2 a b) (node2 c d) m2
-- addDigits2 m1 (One a)        b c (Two   d e)     m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits2 m1 (One a)        b c (Three d e f)   m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits2 m1 (One a)        b c (Four  d e f g) m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits2 m1 (Two a b)      c d (One   e)       m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits2 m1 (Two a b)      c d (Two   e f)     m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits2 m1 (Two a b)      c d (Three e f g)   m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits2 m1 (Two a b)      c d (Four  e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits2 m1 (Three a b c)  d e (One   f)       m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits2 m1 (Three a b c)  d e (Two   f g)     m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits2 m1 (Three a b c)  d e (Three f g h)   m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits2 m1 (Three a b c)  d e (Four  f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits2 m1 (Four a b c d) e f (One   g)       m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits2 m1 (Four a b c d) e f (Two   g h)     m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits2 m1 (Four a b c d) e f (Three g h i)   m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits2 m1 (Four a b c d) e f (Four  g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
--
--
--
-- addDigits3 :: Measured a => FingerTree (Node a) -> Digit a -> a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
-- addDigits3 m1 (One a)        b c d (One   e)       m2 = appendTree2 m1 (node3 a b c) (node2 d e) m2
-- addDigits3 m1 (One a)        b c d (Two   e f)     m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits3 m1 (One a)        b c d (Three e f g)   m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits3 m1 (One a)        b c d (Four  e f g h) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits3 m1 (Two a b)      c d e (One   f)       m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits3 m1 (Two a b)      c d e (Two   f g)     m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits3 m1 (Two a b)      c d e (Three f g h)   m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits3 m1 (Two a b)      c d e (Four  f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits3 m1 (Three a b c)  d e f (One   g)       m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits3 m1 (Three a b c)  d e f (Two   g h)     m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits3 m1 (Three a b c)  d e f (Three g h i)   m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits3 m1 (Three a b c)  d e f (Four  g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits3 m1 (Four a b c d) e f g (One   h)       m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits3 m1 (Four a b c d) e f g (Two   h i)     m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits3 m1 (Four a b c d) e f g (Three h i j)   m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits3 m1 (Four a b c d) e f g (Four  h i j k) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
--
--
--
-- addDigits4 :: Measured a => FingerTree (Node a) -> Digit a -> a -> a -> a -> a -> Digit a -> FingerTree (Node a) -> FingerTree (Node a)
-- addDigits4 m1 (One a)        b c d e (One   f)       m2 = appendTree2 m1 (node3 a b c) (node3 d e f) m2
-- addDigits4 m1 (One a)        b c d e (Two   f g)     m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits4 m1 (One a)        b c d e (Three f g h)   m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits4 m1 (One a)        b c d e (Four  f g h i) m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits4 m1 (Two a b)      c d e f (One   g)       m2 = appendTree3 m1 (node3 a b c) (node2 d e) (node2 f g) m2
-- addDigits4 m1 (Two a b)      c d e f (Two   g h)     m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits4 m1 (Two a b)      c d e f (Three g h i)   m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits4 m1 (Two a b)      c d e f (Four  g h i j) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits4 m1 (Three a b c)  d e f g (One   h)       m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node2 g h) m2
-- addDigits4 m1 (Three a b c)  d e f g (Two   h i)     m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits4 m1 (Three a b c)  d e f g (Three h i j)   m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits4 m1 (Three a b c)  d e f g (Four  h i j k) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
-- addDigits4 m1 (Four a b c d) e f g h (One   i)       m2 = appendTree3 m1 (node3 a b c) (node3 d e f) (node3 g h i) m2
-- addDigits4 m1 (Four a b c d) e f g h (Two   i j)     m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node2 g h) (node2 i j) m2
-- addDigits4 m1 (Four a b c d) e f g h (Three i j k)   m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node2 j k) m2
-- addDigits4 m1 (Four a b c d) e f g h (Four  i j k l) m2 = appendTree4 m1 (node3 a b c) (node3 d e f) (node3 g h i) (node3 j k l) m2
--
-- ----------------
-- -- 4.4 Splitting
-- ----------------
--
-- -- | /O(log(min(i,n-i)))/. Split a sequence at a point where the predicate
-- -- on the accumulated measure changes from 'False' to 'True'.
-- --
-- -- For predictable results, one should ensure that there is only one such
-- -- point, i.e. that the predicate is /monotonic/.
-- split ::  Measured a => (Measure a -> Bool) -> FingerTree a -> (FingerTree a, FingerTree a)
-- split _ Empty               =  (Empty, Empty )
-- split p xs | p (measure xs) =  (l    , x <| r)
--            | otherwise      =  (xs   , Empty )
--   where Split l x r = splitTree p mempty xs
--
-- -- | /O(log(min(i,n-i)))/.
-- -- Given a monotonic predicate @p@, @'takeUntil' p t@ is the largest
-- -- prefix of @t@ whose measure does not satisfy @p@.
-- --
-- -- *  @'takeUntil' p t = 'fst' ('split' p t)@
-- takeUntil :: Measured a => (Measure a -> Bool) -> FingerTree a -> FingerTree a
-- takeUntil p  =  fst . split p
--
-- -- | /O(log(min(i,n-i)))/.
-- -- Given a monotonic predicate @p@, @'dropUntil' p t@ is the rest of @t@
-- -- after removing the largest prefix whose measure does not satisfy @p@.
-- --
-- -- * @'dropUntil' p t = 'snd' ('split' p t)@
-- dropUntil :: Measured a => (Measure a -> Bool) -> FingerTree a -> FingerTree a
-- dropUntil p  =  snd . split p
--
-- data Split t a = Split t a t
--
-- splitTree :: Measured a => (Measure a -> Bool) -> Measure a -> FingerTree a -> Split (FingerTree a) a
-- splitTree _ _ Empty = illegal_argument "splitTree"
-- splitTree _ _ (Single x) = Split Empty x Empty
-- splitTree p i (Deep _ pr m sf)
--   | p vpr       =  let  Split l x r     =  splitDigit p i pr
--                    in   Split (maybe Empty digitToTree l) x (deepL r m sf)
--   | p vm        =  let  Split ml xs mr  =  splitTree p vpr m
--                         Split l x r     =  splitNode p (vpr `mappendVal` ml) xs
--                    in   Split (deepR pr  ml l) x (deepL r mr sf)
--   | otherwise   =  let  Split l x r     =  splitDigit p vm sf
--                    in   Split (deepR pr  m  l) x (maybe Empty digitToTree r)
--   where
--     vpr     =  i    `mappend`  measure pr
--     vm      =  vpr  `mappendVal` m
--
-- -- Avoid relying on right identity (cf Exercise 7)
-- mappendVal :: Measured a => Measure a -> FingerTree a -> Measure a
-- mappendVal v Empty = v
-- mappendVal v t = v `mappend` measure t
--
-- deepL :: Measured a =>
--     Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
-- deepL Nothing m sf      =   rotL m sf
-- deepL (Just pr) m sf    =   deep pr m sf
--
-- deepR :: Measured a =>
--     Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
-- deepR pr m Nothing      =   rotR pr m
-- deepR pr m (Just sf)    =   deep pr m sf
--
-- splitNode :: Measured a => (Measure a -> Bool) -> Measure a -> Node a -> Split (Maybe (Digit a)) a
-- splitNode p i (Node2 _ a b)
--   | p va        = Split Nothing a (Just (One b))
--   | otherwise   = Split (Just (One a)) b Nothing
--   where
--     va      = i `mappend` measure a
-- splitNode p i (Node3 _ a b c)
--   | p va        = Split Nothing a (Just (Two b c))
--   | p vab       = Split (Just (One a)) b (Just (One c))
--   | otherwise   = Split (Just (Two a b)) c Nothing
--   where
--     va      = i `mappend` measure a
--     vab     = va `mappend` measure b
--
-- splitDigit :: Measured a => (Measure a -> Bool) -> Measure a -> Digit a -> Split (Maybe (Digit a)) a
-- splitDigit _ i (One a) = i `seq` Split Nothing a Nothing
-- splitDigit p i (Two a b)
--   | p va        = Split Nothing a (Just (One b))
--   | otherwise   = Split (Just (One a)) b Nothing
--   where
--     va      = i `mappend` measure a
-- splitDigit p i (Three a b c)
--   | p va        = Split Nothing a (Just (Two b c))
--   | p vab       = Split (Just (One a)) b (Just (One c))
--   | otherwise   = Split (Just (Two a b)) c Nothing
--   where
--     va      = i `mappend` measure a
--     vab     = va `mappend` measure b
-- splitDigit p i (Four a b c d)
--   | p va        = Split Nothing a (Just (Three b c d))
--   | p vab       = Split (Just (One a)) b (Just (Two c d))
--   | p vabc      = Split (Just (Two a b)) c (Just (One d))
--   | otherwise   = Split (Just (Three a b c)) d Nothing
--   where
--     va      = i `mappend` measure a
--     vab     = va `mappend` measure b
--     vabc    = vab `mappend` measure c
--
-- ------------------
-- -- Transformations
-- ------------------
--
-- -- | /O(n)/. The reverse of a sequence.
-- reverse :: Measured a => FingerTree a -> FingerTree a
-- reverse = reverseTree id
--
-- reverseTree :: (Measured a2) => (a1 -> a2) -> FingerTree a1 -> FingerTree a2
-- reverseTree _ Empty = Empty
-- reverseTree f (Single x) = Single (f x)
-- reverseTree f (Deep _ pr m sf) =
--     deep (reverseDigit f sf) (reverseTree (reverseNode f) m) (reverseDigit f pr)
--
-- reverseNode :: (Measured a2) => (a1 -> a2) -> Node a1 -> Node a2
-- reverseNode f (Node2 _ a b) = node2 (f b) (f a)
-- reverseNode f (Node3 _ a b c) = node3 (f c) (f b) (f a)
--
-- reverseDigit :: (a -> b) -> Digit a -> Digit b
-- reverseDigit f (One a) = One (f a)
-- reverseDigit f (Two a b) = Two (f b) (f a)
-- reverseDigit f (Three a b c) = Three (f c) (f b) (f a)
-- reverseDigit f (Four a b c d) = Four (f d) (f c) (f b) (f a)
--
-- illegal_argument :: String -> a
-- illegal_argument name =
--     error $ "Logic error: " ++ name ++ " called with illegal argument"
--
-- {- $example
--
-- Particular abstract data types may be implemented by defining
-- element types with suitable 'Measured' instances.
--
-- (from section 4.5 of the paper)
-- Simple sequences can be implemented using a 'Sum' monoid as a measure:
--
-- > newtype Elem a = Elem { getElem :: a }
-- >
-- > instance Measured (Sum Int) (Elem a) where
-- >     measure (Elem _) = Sum 1
-- >
-- > newtype Seq a = Seq (FingerTree (Sum Int) (Elem a))
--
-- Then the measure of a subsequence is simply its length.
-- This representation supports log-time extraction of subsequences:
--
-- > take :: Int -> Seq a -> Seq a
-- > take k (Seq xs) = Seq (takeUntil (> Sum k) xs)
-- >
-- > drop :: Int -> Seq a -> Seq a
-- > drop k (Seq xs) = Seq (dropUntil (> Sum k) xs)
--
-- The module @Data.Sequence@ is an optimized instantiation of this type.
--
-- For further examples, see "Data.IntervalMap.FingerTree" and
-- "Data.PriorityQueue.FingerTree".
--
-- -}
--
--
--
--
--
-- -- | A closed interval.  The lower bound should be less than or equal
-- -- to the higher bound.
-- data Interval v = Interval { len :: v }
--     deriving (Eq, Ord, Show)
--
--
-- data INode v a = INode (Interval v) a deriving (Show, Functor, Foldable, Traversable)
--
-- type instance Measure (INode v a) = IntInterval v
--
-- -- rightmost interval (including largest lower bound) and largest upper bound.
-- data IntInterval v = NoInterval | IntInterval (Interval v) v deriving (Show)
--
-- instance Ord v => Monoid (IntInterval v) where
--     mempty                                             = NoInterval
--     mappend NoInterval          i                      = i
--     mappend i                   NoInterval             = i
--     mappend (IntInterval _ hi1) (IntInterval int2 hi2) = IntInterval int2 (max hi1 hi2)
--
-- instance Ord v => Measured (INode v a) where
--     measure (INode i _) = IntInterval i (high i)
--
-- -- | Map of closed intervals, possibly with duplicates.
-- -- The 'Foldable' and 'Traversable' instances process the intervals in
-- -- lexicographical order.
-- newtype IntervalMap v a = IntervalMap { fromIntervalMap :: FingerTree (INode v a) }
--
-- -- ordered lexicographically by interval
--
-- instance Functor (IntervalMap v) where
--     fmap f (IntervalMap t) = IntervalMap (unsafeFmap (fmap f) t)
--
-- instance Foldable (IntervalMap v) where
--     foldMap f (IntervalMap t) = foldMap (foldMap f) t
--
-- instance Traversable (IntervalMap v) where
--     traverse f (IntervalMap t) =
--         IntervalMap <$> unsafeTraverse (traverse f) t
--
-- -- | 'empty' and 'union'.
-- instance (Ord v) => Monoid (IntervalMap v a) where
--     mempty = IntervalMap mempty
--     mappend (IntervalMap xs) (IntervalMap ys) = IntervalMap (merge1 xs ys)
--       where
--         merge1 as bs = case viewl as of
--             EmptyL                  -> bs
--             a@(INode i _) :< as'     -> l <> (a <| merge2 as' r)
--               where
--                 (l, r) = split larger bs
--                 larger (IntInterval k _) = k >= i
--                 larger NoInterval = error "larger NoInterval"
--         merge2 as bs = case viewl bs of
--             EmptyL                  -> as
--             b@(INode i _) :< bs'     -> l <> (b <| merge1 r bs')
--               where
--                 (l, r) = split larger as
--                 larger (IntInterval k _) = k > i
--                 larger NoInterval = error "larger NoInterval"
--
--
-- -- | /O(1)/.  Interval map with a single entry.
-- singletonx :: Interval v -> a -> IntervalMap v a
-- singletonx i x = IntervalMap (singleton (INode i x))
--
-- -- | /O(log n)/.  Insert an interval into a map.
-- -- The map may contain duplicate intervals; the new entry will be inserted
-- -- before any existing entries for the same interval.
-- insert :: (Ord v) => Interval v -> a -> IntervalMap v a -> IntervalMap v a
-- insert (Interval lo hi) _ m | lo > hi = m
-- insert i x (IntervalMap t) = IntervalMap $ l <> (INode i x <| r)
--   where
--     (l, r) = split larger t
--     larger (IntInterval k _) = k >= i
--     larger NoInterval = error "larger NoInterval"
--
--
-- -- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- -- interval, in lexicographical order.
-- intersections :: (Ord v) => Interval v -> IntervalMap v a -> [(Interval v, a)]
-- intersections i = inRange (low i) (high i)
--
-- -- | /O(k log (n/\//k))/.  All intervals that contain the given interval,
-- -- in lexicographical order.
-- dominators :: (Ord v) => Interval v -> IntervalMap v a -> [(Interval v, a)]
-- dominators i = inRange (high i) (low i)
--
-- -- | /O(k log (n/\//k))/.  All intervals that contain the given point,
-- -- in lexicographical order.
-- search :: (Ord v) => v -> IntervalMap v a -> [(Interval v, a)]
-- search p = inRange p p
--
-- -- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- -- interval, in lexicographical order.
-- inRange :: (Ord v) => v -> v -> IntervalMap v a -> [(Interval v, a)]
-- inRange lo hi (IntervalMap t) = matches (takeUntil (greater hi) t)
--   where
--     matches xs  =  case viewl (dropUntil (atleast lo) xs) of
--         EmptyL    ->  []
--         INode i x :< xs'  ->  (i, x) : matches xs'
--
-- atleast :: (Ord v) => v -> IntInterval v -> Bool
-- atleast k (IntInterval _ hi) = k <= hi
-- atleast _ NoInterval = error "atleast NoInterval"
--
-- greater :: (Ord v) => v -> IntInterval v -> Bool
-- greater k (IntInterval i _) = low i > k
-- greater _ NoInterval = error "greater NoInterval"
--
--
--
--
-- -- Examples
--
-- mkMap :: (Ord v) => [(v, v, a)] -> IntervalMap v a
-- mkMap = foldr ins mempty
--   where
--     ins (lo, hi, n) = insert (Interval lo hi) n
--
--
-- maths :: IntervalMap Int String
-- maths = mkMap [ (0 , 100, "A")
--               , (30, 130, "B")
--               , (60, 160, "C")
--               ]
--
-- main :: IO ()
-- main = do
--     -- print $ search 40 maths
--     pprint $ takeUntil (greater 40) (fromIntervalMap maths)
--














-- maths :: IntervalMap Int String
-- maths = mkMap [
--     (1642, 1727, "Newton"),
--     (1646, 1716, "Leibniz"),
--     (1707, 1783, "Euler"),
--     (1736, 1813, "Lagrange"),
--     (1777, 1855, "Gauss"),
--     (1811, 1831, "Galois")]

--
-- newtype Elem a = Elem { getElem :: a }
-- type instance Measure (Elem a) = Sum Int
--
-- instance Show a => Show (Elem a) where
--     show (Elem a) = show a
--
-- instance Measured (Elem a) where
--     measure (Elem _) = Sum 1
--
-- newtype Seq a = Seq (FingerTree (Elem a)) deriving (Show)
--
-- takex :: Int -> Seq a -> Seq a
-- takex k (Seq xs) = Seq (takeUntil (> Sum k) xs)
--
-- dropx :: Int -> Seq a -> Seq a
-- dropx k (Seq xs) = Seq (dropUntil (> Sum k) xs)
--
--
-- main :: IO ()
-- main = do
--     let e = Seq $ fromList (Elem <$> ["g", "h", "a", "c", "b", "k"]) :: Seq String
--     print e
--     print $ takex 2 e










--
--
--
-- -- | A closed interval.  The lower bound should be less than or equal
-- -- to the higher bound.
-- data Interval v = Interval { low :: v, high :: v }
--     deriving (Eq, Ord, Show)
--
-- -- | An interval in which the lower and upper bounds are equal.
-- point :: v -> Interval v
-- point v = Interval v v
--
-- data INode v a = INode (Interval v) a deriving (Show, Functor, Foldable, Traversable)
--
-- type instance Measure (INode v a) = IntInterval v
--
-- -- rightmost interval (including largest lower bound) and largest upper bound.
-- data IntInterval v = NoInterval | IntInterval (Interval v) v deriving (Show)
--
-- instance Ord v => Monoid (IntInterval v) where
--     mempty                                             = NoInterval
--     mappend NoInterval          i                      = i
--     mappend i                   NoInterval             = i
--     mappend (IntInterval _ hi1) (IntInterval int2 hi2) = IntInterval int2 (max hi1 hi2)
--
-- instance Ord v => Measured (INode v a) where
--     measure (INode i _) = IntInterval i (high i)
--
-- -- | Map of closed intervals, possibly with duplicates.
-- -- The 'Foldable' and 'Traversable' instances process the intervals in
-- -- lexicographical order.
-- newtype IntervalMap v a = IntervalMap { fromIntervalMap :: FingerTree (INode v a) }
--
-- -- ordered lexicographically by interval
--
-- instance Functor (IntervalMap v) where
--     fmap f (IntervalMap t) = IntervalMap (unsafeFmap (fmap f) t)
--
-- instance Foldable (IntervalMap v) where
--     foldMap f (IntervalMap t) = foldMap (foldMap f) t
--
-- instance Traversable (IntervalMap v) where
--     traverse f (IntervalMap t) =
--         IntervalMap <$> unsafeTraverse (traverse f) t
--
-- -- | 'empty' and 'union'.
-- instance (Ord v) => Monoid (IntervalMap v a) where
--     mempty = IntervalMap mempty
--     mappend (IntervalMap xs) (IntervalMap ys) = IntervalMap (merge1 xs ys)
--       where
--         merge1 as bs = case viewl as of
--             EmptyL                  -> bs
--             a@(INode i _) :< as'     -> l <> (a <| merge2 as' r)
--               where
--                 (l, r) = split larger bs
--                 larger (IntInterval k _) = k >= i
--                 larger NoInterval = error "larger NoInterval"
--         merge2 as bs = case viewl bs of
--             EmptyL                  -> as
--             b@(INode i _) :< bs'     -> l <> (b <| merge1 r bs')
--               where
--                 (l, r) = split larger as
--                 larger (IntInterval k _) = k > i
--                 larger NoInterval = error "larger NoInterval"
--
--
-- -- | /O(1)/.  Interval map with a single entry.
-- singletonx :: Interval v -> a -> IntervalMap v a
-- singletonx i x = IntervalMap (singleton (INode i x))
--
-- -- | /O(log n)/.  Insert an interval into a map.
-- -- The map may contain duplicate intervals; the new entry will be inserted
-- -- before any existing entries for the same interval.
-- insert :: (Ord v) => Interval v -> a -> IntervalMap v a -> IntervalMap v a
-- insert (Interval lo hi) _ m | lo > hi = m
-- insert i x (IntervalMap t) = IntervalMap $ l <> (INode i x <| r)
--   where
--     (l, r) = split larger t
--     larger (IntInterval k _) = k >= i
--     larger NoInterval = error "larger NoInterval"
--
--
-- -- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- -- interval, in lexicographical order.
-- intersections :: (Ord v) => Interval v -> IntervalMap v a -> [(Interval v, a)]
-- intersections i = inRange (low i) (high i)
--
-- -- | /O(k log (n/\//k))/.  All intervals that contain the given interval,
-- -- in lexicographical order.
-- dominators :: (Ord v) => Interval v -> IntervalMap v a -> [(Interval v, a)]
-- dominators i = inRange (high i) (low i)
--
-- -- | /O(k log (n/\//k))/.  All intervals that contain the given point,
-- -- in lexicographical order.
-- search :: (Ord v) => v -> IntervalMap v a -> [(Interval v, a)]
-- search p = inRange p p
--
-- -- | /O(k log (n/\//k))/.  All intervals that intersect with the given
-- -- interval, in lexicographical order.
-- inRange :: (Ord v) => v -> v -> IntervalMap v a -> [(Interval v, a)]
-- inRange lo hi (IntervalMap t) = matches (takeUntil (greater hi) t)
--   where
--     matches xs  =  case viewl (dropUntil (atleast lo) xs) of
--         EmptyL    ->  []
--         INode i x :< xs'  ->  (i, x) : matches xs'
--
-- atleast :: (Ord v) => v -> IntInterval v -> Bool
-- atleast k (IntInterval _ hi) = k <= hi
-- atleast _ NoInterval = error "atleast NoInterval"
--
-- greater :: (Ord v) => v -> IntInterval v -> Bool
-- greater k (IntInterval i _) = low i > k
-- greater _ NoInterval = error "greater NoInterval"
--
--
--
--
-- -- Examples
--
-- mkMap :: (Ord v) => [(v, v, a)] -> IntervalMap v a
-- mkMap = foldr ins mempty
--   where
--     ins (lo, hi, n) = insert (Interval lo hi) n
--
-- composers :: IntervalMap Int String
-- composers = mkMap [
--     (1685, 1750, "Bach"),
--     (1685, 1759, "Handel"),
--     (1732, 1809, "Haydn"),
--     (1756, 1791, "Mozart"),
--     (1770, 1827, "Beethoven"),
--     (1782, 1840, "Paganini"),
--     (1797, 1828, "Schubert"),
--     (1803, 1869, "Berlioz"),
--     (1810, 1849, "Chopin"),
--     (1833, 1897, "Brahms"),
--     (1838, 1875, "Bizet")]
--
-- maths :: IntervalMap Int String
-- maths = mkMap [ (0 , 100, "A")
--               , (30, 130, "B")
--               , (60, 160, "C")
--               ]
--
-- main :: IO ()
-- main = do
--     -- print $ search 40 maths
--     pprint $ takeUntil (greater 40) (fromIntervalMap maths)
--
