
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
--{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoOverloadedStrings       #-}

-- {-# LANGUAGE PolyKinds #-}

module Main where

import           Prologue hiding (Cons, Indexable, Ixed, Repr, Simple, children, cons, empty, index, lookup, maxBound, minBound, repr,
                           simple, ( # ))

main = do
    print "hello"
    print "hello"


--import Data.Repr

--import qualified Data.Map            as Map
--import           Data.Map            (Map)
--import qualified Data.Text.Lazy      as Text
--import           Data.Text.Lazy      (Text)
--import           GHC.Prim (Any)
--import           GHC.Int
--import           Unsafe.Coerce (unsafeCoerce)
--import           Data.Convert
--import qualified Data.IntMap.Lazy as IntMap
--import           Data.IntMap.Lazy (IntMap)
--import Data.Typeable       hiding (cast)
--import qualified Control.Monad.State as State
--import Data.Vector (Vector)
--import qualified Data.Vector as Vector
--import Data.Vector.Mutable ()
--import Data.Maybe (fromJust)
--import System.Process
--import qualified Data.Text.AutoBuilder as Text
--import Data.Map (Map)
--import qualified Data.Map as Map
--import Data.Constraint
--import Control.Error.Util (hush)
--import Data.Convert.Errors (TypeMismatch (TypeMismatch))
--import Data.Constraint.Void
----import Data.Variants hiding (cons)
----import qualified Data.Variants as V
--import Flowbox.System.Types hiding ((.:), insert, Index)
--import           Control.Monad.State.Generate (newState)
--import Text.Read (readMaybe)
--import           Luna.Syntax.Repr.Graph
--import qualified Luna.Syntax.Repr.Graph as GraphBuilder
--import           Luna.Syntax.Builder
--import qualified Luna.Syntax.Builder    as Builder
--import qualified Luna.Syntax.Builder.Class as Builder
--import           Luna.Syntax.AST.Term
--import           Luna.Syntax.AST
--import           Luna.Syntax.AST.Decl
--import           Luna.Syntax.Name.Pool
--import Control.Monad.Fix
--import Data.Cata
--import           Luna.Syntax.Builder.Star (MonadStarBuilder)
--import           Luna.Syntax.Builder.Star (StarBuilder, StarBuilderT)
--import qualified Luna.Syntax.Builder.Star as StarBuilder
--import Control.Monad.Trans.Identity
----import Luna.Diagnostic.AST (toGraphViz, display)
----import Luna.Diagnostic.AST as Diag (toGraphViz, display, render, open)
--import Luna.Syntax.AST.Typed
--import Luna.Syntax.Layer.Labeled
--import qualified Type.BaseType as BT
----import Data.Container
----import Data.Container.Hetero
--import Data.Container.Resizable
--import Data.Container.Reusable
----import Data.Container.Interface
----import           Data.Container.Poly {- x -} hiding (append)
--import Data.Container
--import Data.Container.Poly -- (Ixed)
----import Data.Text.CodeBuilder.Builder
--import Data.Text.CodeBuilder.Builder as CB hiding (render, app)

--import Data.Vector.Dynamic as VD

--import Data.Container.Parametrized
--import Data.Container.Auto
--import Data.Container.Weak
--import qualified Data.Container.Opts as Mods
--import qualified Data.Container.Instances.Vector.Lazy as Lazy



--import Data.STRef
--import Control.Monad.ST
--import Data.Reprx
--import Data.Layer
--import qualified System.Mem.Weak      as Mem
--import Data.IORef

--import Data.Container.Immersed
--import Data.Container.Hetero (Ptr(Ptr), ptrIdx)

--import Data.Container.Hetero
----import Data.Layer.Coat
--import qualified Luna.Syntax.Builder.Node as NodeBuilder
--import           Luna.Syntax.Builder.Node (MonadNodeBuilder)
--import qualified Data.IntSet as IntSet
--import           Data.IntSet (IntSet)
--import           Data.Construction

--import Luna.Syntax.AST
--import Data.Variant
--import Data.Variant.Layers
--import Data.Variant.Cons
--import Data.Variant.Patterns (case', ANY(ANY))
--import Data.Attributes

--import Data.Result

--import Data.Variant.Properties

--import Luna.Syntax.Builder.Cache
--import Data.Variant.Patterns (ANY, MatchSet, MatchResolver, resolveMatch)
--import qualified Luna.Syntax.AST.Layout as Layout

----import Luna.Syntax.AST.Cache.Lit

----foo :: Lit Int IDT -> String
----foo l = case' l $ do
----            matchStar' $ \Star   -> ("Star"            :: String)
----            matchAny'  $ \ANY    -> ("Something else" :: String)


--import System.Environment (getArgs)



----matchStar :: (Star -> a) -> MatchSet (Lit Int IDT) a

----foo_xy = cons ∘ Str

--import qualified Luna.Syntax.Builder.Cache.Val as CV



--testmatch l = case' l $ do
--    match $ \Star   -> print ("OH!" :: String) --("Star"            :: String)
--    match $ \ANY    -> return () -- ("Something else" :: String)


--testmatch2 d = case d of
--    DD2 -> print ("OH" :: String)
--    _   -> return ()

--data DD = DD1 String
--        | DD2
--        | DD3
--        deriving (Show)


--foo1 :: Val Layout.Static Int IDT -> MatchSet (Val Layout.Static Int IDT) String -> String


--main :: IO ()
--main = do


--    --args <- getArgs
--    --let mode   = read (args !! 0) :: Int
--    --    argnum = read (args !! 1) :: Int
--    --    nums = [0..argnum]

--    --print "hello"

--    --print args
--    ----print nums
--    let l = str  "foo" :: Lit Int IDT
--        v = l +> (Raw (5 :: Int) @: (Prop # Value))
--        d = cons v :: Static Val Int IDT
--    --print $ case' l $ do
--    --    match $ \Star   -> ("Star"           :: String)
--    --    match $ \ANY    -> ("Something else" :: String)
--    print l


--    --case mode of
--    --    0 -> do
--    --        let ls   = str <$> (show <$> nums) :: [Lit Int IDT]
--    --        mapM_ testmatch ls
--    --    1 -> do
--    --        let ls   = DD1 <$> (show <$> nums)
--    --        mapM_ testmatch2 ls


--    --let u :: MatchSet (Val Layout.Static Int IDT) String
--    --    u = CV.matchStaticLit $ \(Lit l :: Lit Int IDT) -> ("Lit"            :: String)

--    --print "done!"

--    --print u

--    print $ case' l $ do
--        match $ \Star    -> ("Star"           :: String)
--        match $ \(Str s) -> ("Str: " <> s     :: String)
--        match $ \ANY     -> ("Something else" :: String)


--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    print $ case' d $ do
--        match $ \(Lit l) -> ("foo " :: String)

--    --print $ foo1 d u



--    return ()



---- === HomoBuilder ===

----newtype HomoG (t :: * -> *) m a = HomoG { fromHomoG :: IdentityT m a } deriving (MonadFix, Monad, Functor, Applicative, MonadTrans)

----runHomoBuilder :: HomoG t m a -> m a
----runHomoBuilder = runIdentityT . fromHomoG


----instance (MuBuilder a m t, t ~ t') => MuBuilder a (HomoG t m) t' where
----    buildMu = lift . buildMu

----type LibMap = Map String (Ref Node)

----type Network = Graph (Labeled2 Int (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) DoubleArc

----typed a t = StarBuilder.with (const $ Just t) a

----addStdLiterals :: Network -> (LibMap, Network)
----addStdLiterals g = runIdentity
----                 $ flip StarBuilder.evalT Nothing
----                 $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
----                 $ flip Builder.runT g
----                 $ mdo
----    strLit <- string "String" `typed` (Ref $ Node 0)
----    strTp  <- cons strLit
----    reconnect strLit tp strTp

----    intLit <- string "Int" `typed` strTp
----    intTp  <- cons intLit

----    return $ Map.insert "String" strTp
----           $ Map.insert "Int"    intTp
----           $ Map.empty

----type H t a = t (a t)



------tstx2 :: (Node' (HRef2 Coat (Draft2 (HRef2 Coat))), Graph2)
----tstx2 :: (Node' (H (HRef2 (Labeled Int (Typed2 Coat))) Draft2), Graph2)
----tstx2 = runIdentity
----      $ flip StarBuilder.evalT Nothing
----      $ flip Builder.runT def
----      $ flip NodeBuilder.evalT (Node' $ HRef2 0 :: Node' (H (HRef2 (Labeled Int (Typed2 Coat))) Draft2))
----      $ string2 "hello"

----tstx1 :: ((), Network)
----tstx1 = runIdentity
----      $ flip StarBuilder.evalT Nothing
----      $ flip Builder.runT def
----      $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
----      $ do
----            topStar <- getStar2
----            i1 <- int 7
----            s  <- string "hello"
----            --i1 <- _int 5
----            --i1 <- _int 4
----            --i2 <- _int 3
----            --str <- _string "plus"
----            --acc <- accessor "plus" i1
----            --str <- _string "plus"
----            --s <- getStar2
----            --i1 <- _int 4
----            --i1 <- _int 4
----            --i1 <- _star
----            return ()


----pass2 :: LibMap -> Network -> ([Ref Node], Network)
----pass2 lmap gr = runIdentity
----              $ flip StarBuilder.evalT Nothing
----              $ flip Builder.runT gr
----              $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
----              $ do
----    --let cptr       = ptrIdx . fromRef . unwrap
----    let Just strTp = Map.lookup "String" lmap
----    let Just intTp = Map.lookup "Int"    lmap
----    g <- Builder.get
----    let ptrs = Ref . Node <$> usedIxes (g ^. nodes) :: [Ref Node]

----    let process (ref :: Ref Node) = do
----        node <- readRef ref
----        let procnod ast = case' ast $ do
----                match $ \case
----                    Int _ -> do
----                        tnode <- follow $ node ^. tp
----                        uni   <- unify tnode intTp
----                        reconnect ref tp uni
----                        return [uni]
----                        -- FIXME: poprawic wkladanie unify - unify powinno "inplace" zastepowac node, nie przepinac go

----                    _     -> return []
----                match $ \(Val a :: Val (Ref Edge)) -> procnod (V.cast $ Val a)
----                match $ \ANY -> return []
----        procnod (uncoat node)

----    unis <- concat <$> mapM process ptrs
----    return unis


---- unifikacja lewo i obustronna!
---- najlepiej chyba wprowadzc skoki zliczajace poprzednikow


----pass3 :: LibMap -> [Ref Node] -> Network -> ((), Network)
----pass3 lmap unis gr = runIdentity
----                   $ flip StarBuilder.evalT Nothing
----                   $ flip Builder.runT gr
----                   $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
----                   $ do
----    flip mapM_ unis $ \ uni -> do
----        node <- readRef uni
----        let Unify a b = unsafeFrom $ uncoat node
----        a' <- follow a
----        b' <- follow b
----        na <- readRef a'
----        nb <- readRef b'
----        atp' <- follow . view tp =<< readRef a'
----        --destruct atp'
----        case' (uncoat na) $ do
----            match $ \Star -> do
----                let Labeled2 _ (Typed t (SuccTracking ss (Coat ast))) = node
----                mapM_ (retarget b') (Ref . Edge <$> toList ss)
----                --destruct a'
----                --mapM_ (retarget $ Ref $ Node 0) (Ref . Edge <$> toList ss)
----                return ()
----            match $ \ANY  -> return ()
----        destruct uni
----        return ()
----    return ()

----runUniqM :: State.MonadState IntSet m => Int -> m () -> m ()
----runUniqM a f = do
----    s <- State.get
----    if IntSet.member a s then return ()
----                         else State.put (IntSet.insert a s) >> f
------retarget tgt edge = withRef edge $ target .~ tgt



----case' :: (IsMatchSet matches (RecordOf rec) out, IsRecord rec) => rec -> matches -> out
----case' = unsecureCase . view record


----Match' rec v   where match :: (v -> a) -> MatchSet rec a


----bar :: Int
----bar = 5

----main :: IO ()
----main = do
----    print "hello"


----    let l    = case'_xy  "foo" :: Lit Int IDT


----        --v   = l +> ((5 :: Int) @: (Prop # Value))
----        --d   = cons v :: Static Val Int IDT

----    print l

----    print bar







----    print $ case' l $ do
----        matchStar $ \Star    -> ("Star"            :: String)
--        --(match :: (Star -> String) -> MatchSet (RecordOf (Lit Int IDT)) String) $ \Star    -> ("Star"            :: String)
--    --    --match $ \(Str s) -> ("String: " <> s   :: String)
--    --    --match $ \(Str s) -> ("String: " <> s   :: String)
--    --    --match $ \(Str s) -> ("String: " <> s   :: String)
--        --matchAny $ \ANY     -> ("something else!" :: String)



--    --print $ case' d $ do
--    --    match $ \(Cons _ _) -> ("Its Cons!"       :: String)
--    --    match $ \(Lit l)    -> ("Its Lit!"        :: String)
--    --    match $ \ANY        -> ("something else!" :: String)


--    --case' l1 $ do
--    --    match
--    --let g  = snd tstx1
--    --let (lmap, gs) = addStdLiterals g
--    --let (unis, g2) = pass2 lmap gs
--    --let (_   , g3) = pass3 lmap unis g2

--    --renderAndOpen [ ("g" , g)
--    --            --   , ("gs", gs)
--    --            --   , ("g2", g2)
--    --            --   , ("g3", g3)
--    --              ]

--    --pprint g2

--    --print "endxs1"


--    --let (unis, g)  = exDbg1
--    --let (_   , g3) = pass3 undefined unis g

--    --renderAndOpen [
--    --                ("g" , g)  ,
--    --                --("gs", gs) ,
--    --                --("g2", g2) ,
--    --                ("g3", g3)
--    --              ]

--    --pprint (zip [0..] $ elems (g ^. nodes))
--    --pprint (zip [0..] $ elems (g ^. edges))

--    --render "g"  $ toGraphViz g
--    --render "gs" $ toGraphViz gs
--    --render "g2" $ toGraphViz g2

--    --open $ fmap (\i -> "/tmp/t" <> show i <> ".png") $ reverse [1..2]

--    --pprint g2


--    --print tstmv`

--    --putStrLn $ repr y
--    --print . repr =<< nytst2
--        --let (s, g) = nytst2

--        --print $ ixes g
--        --print $ usedIxes g
--        --print $ freeIxes g

--        --let (freg, g') = addStdLiterals s g

--        ----print   gv

--        --let (unis2, g2) = pass2 freg s g'
--        --let (_    , g3) = pass3 unis2 s g2

--        --render "t1" $ toGraphViz g'
--        --render "t2" $ toGraphViz g2
--        --render "t3" $ toGraphViz g3

--        --open $ fmap (\i -> "/tmp/t" <> show i <> ".png") [1..3]
--    --let xa = fromList [1,2,3] :: Auto (Weak Vector) Int
--    --let xb = fromList [1,2,3] :: WeakAuto Vector Int
--    --let xa = fromList [1,2,3] :: Weak Vector Int
--    --xc <- addM 5 xb
--    --print $ elems xb
--    --print $ (elems xa :: [Int])
--    --print $ unlayer xa
--    --print $ elems xa
--    --Lazy.main
--    --return ()



----renderAndOpen lst = do
----    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
----    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)



----data L1 a = L1 a deriving (Show)
----type instance (Container (L1 a)) = Container a
----instance (HasContainerM m a, Functor m) => HasContainerM m (L1 a)

----instance (IsContainerM m a, Functor m) => IsContainerM m (L1 a) where fromContainerM = fmap L1 . fromContainerM
----instance Wrapped (L1 a) where
----    type Unwrapped (L1 a) = a
----    _Wrapped' = iso (\(L1 a) -> a) L1




----type instance Container (IORef a) = Container a
----instance (HasContainerM m a, MonadIO m) => HasContainerM m (IORef a) where
----    viewContainerM   ref = viewContainerM =<< liftIO (readIORef ref)
----    setContainerM  v ref = ref <$ (liftIO (readIORef ref) >>= setContainerM v >>= liftIO . writeIORef ref)

----instance (IsContainerM m a, MonadIO m) => IsContainerM m (IORef a) where
----    fromContainerM a = liftIO . newIORef =<< fromContainerM a

----type instance Unlayered (IORef a) = a
----instance MonadIO m => LayeredM m (IORef a) where
----    viewLayeredM    = liftIO . readIORef
----    setLayeredM a l = l <$ liftIO (writeIORef l a)

----xxxt :: Ixed Appendable Int a => a -> (a, Index (Container a))
----xxxt v = ixed append (4 :: Int) v

----xxxt2 :: Appendable Int a => a -> a
----xxxt2 v = append (4 :: Int) v

----type TT = Vector Int



----data HSIndent  = HSIndent  deriving (Show)


----runMeI = renderCode HSIndent


----instance Repr s (VectorGraph a) where repr _ = fromString "mu"




----checkNothing a = if (a == Nothing) then 1 else 0




----data MR = MR
----data MI = MI
----data MX = MX

----data Bundle a b = Bundle a b

----b = flip Bundle undefined

----class Foom mods where
----    foom :: mods -> mods


----t1 = foom (b MR, (b MI, (b MX, ())))


----class SetMod mod a query where setMod :: Bundle mod a -> query -> query
----instance {-# OVERLAPPABLE #-} (a ~ a')        => SetMod mod a (Bundle mod  a', qs) where setMod b (_, qs) = (b,qs)
----instance {-# OVERLAPPABLE #-} SetMod mod a qs => SetMod mod a (Bundle mod' a', qs) where setMod b (q, qs) = (q, setMod b qs)
