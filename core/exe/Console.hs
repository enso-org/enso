{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoOverloadedStrings  #-}


module Main where


import qualified Data.ByteString.Lazy.Char8 as ByteString

import Luna.Prelude hiding (String, typeRep, cons, elem)
import Data.Aeson (encode)

import           Luna.IR
import qualified Luna.IR.Repr.Vis as Vis
import           Luna.IR.Repr.Vis (MonadVis, snapshot)
import           Luna.Pass        (Pass, Inputs, Outputs, Preserves)
import qualified Luna.Pass        as Pass
import           Luna.IR.Expr.Layout.Nested ((:>))
import qualified Luna.IR.Expr.Layout.ENT as Layout

import Web.Browser (openBrowser )

import Luna.IR.Expr.Term.Class
import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad.State (MonadState, StateT, execStateT, get, put)
import qualified Control.Monad.State as State




data Incoherence = DeteachedSource (Expr Draft) (Link' (Expr Draft))
                 | OrphanLink      (Link' (Expr Draft))
                 | OrphanExpr      (Expr Draft)
                 deriving (Show)

data CoherenceCheck = CoherenceCheck { _incoherences   :: [Incoherence]
                                     , _uncheckedLinks :: Set (Link' (Expr Draft))
                                     , _orphanExprs    :: Set (Expr Draft)
                                     } deriving (Show)



makeLenses ''CoherenceCheck

finalize :: CoherenceCheck -> [Incoherence]
finalize s = s ^. incoherences <> (OrphanLink <$> Set.toList (s ^. uncheckedLinks)) <> (OrphanExpr <$> Set.toList (s ^. orphanExprs))


type MonadCoherenceCheck m = (MonadState CoherenceCheck m, CoherenceCheckCtx m)
type CoherenceCheckCtx   m = (IRMonad m, Readables m '[ExprLayer Model, ExprLayer Type, ExprLinkLayer Model, ExprNet, ExprLinkNet])

reportIncoherence :: MonadCoherenceCheck m => Incoherence -> m ()
reportIncoherence i = State.modify (incoherences %~ (i:))

markLinkChecked :: MonadCoherenceCheck m => Link' (Expr Draft) -> m ()
markLinkChecked a = State.modify (uncheckedLinks %~ (Set.delete a))

markExprChecked :: MonadCoherenceCheck m => Expr Draft -> m ()
markExprChecked a = State.modify (orphanExprs %~ (Set.delete a))



checkCoherence :: CoherenceCheckCtx m => m [Incoherence]
checkCoherence = do
    es <- exprs
    ls <- links
    s <- flip execStateT (CoherenceCheck def (Set.fromList ls) (Set.fromList es)) $ do
        mapM_ checkExprCoherence es
        mapM_ checkExprExistence ls
    return $ finalize s

checkExprExistence :: MonadCoherenceCheck m => Link' (Expr Draft) -> m ()
checkExprExistence lnk = do
    (_, tgt) <- readLayer @Model lnk
    markExprChecked tgt

checkExprCoherence :: MonadCoherenceCheck m => Expr Draft -> m ()
checkExprCoherence e = do
    tp <- readLayer @Type e
    checkLinkTarget e tp
    mapM_ (checkLinkTarget e) =<< symbolFields e


checkLinkTarget :: MonadCoherenceCheck m => Expr Draft -> Link' (Expr Draft) -> m ()
checkLinkTarget e lnk = do
    markLinkChecked lnk
    (_, tgt) <- readLayer @Model lnk
    when (e ^. elem . idx /= tgt ^. elem . idx) $ reportIncoherence (DeteachedSource e lnk)












data MyData = MyData Int deriving (Show)

data                    SimpleAA
type instance Inputs    SimpleAA = '[Attr MyData, ExprNet, ExprLinkNet] <> ExprLayers '[Model, UID, Type] <> ExprLinkLayers '[Model, UID]
type instance Outputs   SimpleAA = '[Attr MyData, ExprNet, ExprLinkNet] <> ExprLayers '[Model, UID, Type] <> ExprLinkLayers '[Model, UID]
type instance Preserves SimpleAA = '[]

pass1 :: (MonadFix m, MonadIO m, IRMonad m, MonadVis m) => Pass SimpleAA m
pass1 = gen_pass1

test_pass1 :: (MonadIO m, MonadFix m, PrimMonad m, MonadVis m) => m (Either Pass.Err ())
test_pass1 = runIRT $ do
    runRegs

    attachLayer (typeRep @Model) (typeRep @EXPR)
    attachLayer (typeRep @Succs) (typeRep @EXPR)
    attachLayer (typeRep @Type)  (typeRep @EXPR)
    attachLayer (typeRep @UID)   (typeRep @EXPR)

    attachLayer (typeRep @Model) (typeRep @(LINK' EXPR))
    attachLayer (typeRep @UID)   (typeRep @(LINK' EXPR))

    setAttr $ MyData 7

    Pass.eval pass1


uncheckedDeleteStar :: (IRMonad m, Readable (ExprLayer Type) m, Accessibles m '[ExprLinkNet, ExprNet]) => Expr l -> m ()
uncheckedDeleteStar e = do
    delete =<< readLayer @Type e
    delete e
{-# INLINE uncheckedDeleteStar #-}

uncheckedDeleteStarType :: (IRMonad m, Readable (ExprLayer Type) m, Accessibles m '[ExprLinkNet, ExprNet, ExprLinkLayer Model])
                        => Expr l -> m ()
uncheckedDeleteStarType e = do
    typeLink <- readLayer @Type e
    (oldStar, _) <- readLayer @Model typeLink
    uncheckedDeleteStar oldStar
    delete typeLink
{-# INLINE uncheckedDeleteStarType #-}


gen_pass1 :: ( MonadIO m, IRMonad m, MonadVis m
             , Accessibles m '[ExprLayer Model, ExprLinkLayer Model, ExprLayer Type, ExprLinkLayer UID, ExprLayer UID, ExprNet, ExprLinkNet, Attr MyData]
             ) => m ()
gen_pass1 = layouted @Ent $ do
    -- (s :: Expr Star) <- star


    -- Str constructor
    (strName :: Expr (ET' String)) <- rawString "String"
    (strCons :: Expr (NT' (Cons :> ET' String))) <- cons strName
    snapshot "s1"
    let strCons' = unsafeRelayout strCons :: Expr Layout.Cons'
        strName' = unsafeRelayout strName :: Expr (ET String Layout.Cons')
    newTypeLink <- link strCons' strName'
    uncheckedDeleteStarType strName'
    writeLayer @Type newTypeLink strName'
    snapshot "s2"

    let string s = do
            foo <- rawString s
            snapshot "s3"
            let foo' = unsafeRelayout foo :: Expr (ET String Layout.Cons')
            ftlink <- link strCons' foo'
            uncheckedDeleteStarType foo'
            writeLayer @Type ftlink foo'
            return foo'

    s <- string "s1"
    s <- string "s2"
    s <- string "s3"

    print =<< checkCoherence
    snapshot "s4"


    -- (a :: Expr Int Star)) <- var aName
    -- b <- var "b"

    -- (u :: Expr (ENT _ _ _)) <- unify a b
    -- -- (f :: Expr (ENT Star Star Star)) <- acc "f" u
    --
    --
    --
    -- -- snapshot "s3"
    -- d <- readLayer @Type u
    -- print d
    --
    --
    -- md <- readAttr @MyData
    -- print md
    --
    -- ts <- exprs
    -- print ts
    --
    -- match s $ \case
    --     Unify l r -> print "ppp"
    --     Star      -> match s $ \case
    --         Unify l r -> print "hola"
    --         Star      -> print "hellox"
    --
    -- print "---"
    --
    --
    -- match a $ \ (Var l) -> do
    --     n <- source l
    --     match n $ \case
    --         String s -> print s
    --
    -- v <- var "ala"
    -- n <- strName v
    -- (s3 :: Expr (ENT Draft String Draft)) <- generalize <$> star
    -- (v3 :: Expr (ENT Draft String Draft)) <- generalize <$> var "lel"
    -- match v3 $ \(Var l) -> do
    --     print "IT'S VAR"
    --     n' <- source l
    --     n  <- match n' $ \case
    --         String s -> return s
    --     print n
    --
    -- print n

    return ()






main :: IO ()
main = do
    (p, vis) <- Vis.newRunDiffT test_pass1
    case p of
        Left e -> do
            print "* INTERNAL ERROR *"
            print e
        Right _ -> do
            let cfg = ByteString.unpack $ encode $ vis
            -- putStrLn cfg
            liftIO $ openBrowser ("http://localhost:8000?cfg=" <> cfg)
            return ()
    print p
    return ()
