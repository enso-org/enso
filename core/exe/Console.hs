{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoOverloadedStrings  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}


module Main where


import qualified Data.ByteString.Lazy.Char8 as ByteString

import Luna.Prelude as Prelude hiding (String, cons, elem)
import Data.Aeson (encode)

import           Luna.IR
import qualified Luna.IR.Repr.Vis as Vis
import           Luna.IR.Repr.Vis (MonadVis)
import           Luna.Pass        (Pass, Inputs, Outputs, Events, Preserves, SubPass)
import qualified Luna.Pass        as Pass
import           Luna.IR.Expr.Layout.Nested (type (>>))
import           Luna.IR.Expr.Layout.ENT (type (:>), type (#>), String')
import qualified Luna.IR.Expr.Layout.ENT as Layout

import Web.Browser (openBrowser )

import Luna.IR.Expr.Term.Class
import qualified Data.Set as Set
import Data.Set (Set)

import Control.Monad.State (MonadState, StateT, execStateT, get, put)
import qualified Control.Monad.State as State

import Data.RTuple (Assoc ((:=)))
import Luna.Pass.Manager as PM

import Data.Event as Event

import Data.Reflection

import Data.TypeVal
import qualified Luna.IR.Internal.LayerStore as Store
import Luna.IR.Internal.LayerStore (STRefM)
import Luna.IR.Layer.UID (ID)

import qualified Luna.IR.Expr.Term.Named as Term


import qualified Data.RTuple.Class as RT
import Data.Property

import System.Log




-- BEGIN == WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP ===

--------------------------------
-- === IR coherence check === --
--------------------------------

data Incoherence = DeteachedSource AnyExpr AnyExprLink
                 | OrphanLink      AnyExprLink
                 | OrphanExpr      AnyExpr
                 deriving (Show)

data CoherenceCheck = CoherenceCheck { _incoherences   :: [Incoherence]
                                     , _uncheckedLinks :: Set AnyExprLink
                                     , _orphanExprs    :: Set AnyExpr
                                     } deriving (Show)



makeLenses ''CoherenceCheck

finalize :: CoherenceCheck -> [Incoherence]
finalize s = s ^. incoherences <> (OrphanLink <$> Set.toList (s ^. uncheckedLinks)) <> (OrphanExpr <$> Set.toList (s ^. orphanExprs))


type MonadCoherenceCheck m = (MonadState CoherenceCheck m, CoherenceCheckCtx m)
type CoherenceCheckCtx   m = (IRMonad m, Readables m '[ExprLayer Model, ExprLayer Type, ExprLinkLayer Model, ExprNet, ExprLinkNet])

reportIncoherence :: MonadCoherenceCheck m => Incoherence -> m ()
reportIncoherence i = State.modify (incoherences %~ (i:))

markLinkChecked :: MonadCoherenceCheck m => AnyExprLink -> m ()
markLinkChecked a = State.modify (uncheckedLinks %~ (Set.delete a))

markExprChecked :: MonadCoherenceCheck m => AnyExpr -> m ()
markExprChecked a = State.modify (orphanExprs %~ (Set.delete a))



checkCoherence :: CoherenceCheckCtx m => m [Incoherence]
checkCoherence = do
    es <- exprs
    ls <- links
    s <- flip execStateT (CoherenceCheck def (Set.fromList ls) (Set.fromList es)) $ do
        mapM_ checkExprCoherence es
        mapM_ checkExprExistence ls
    return $ finalize s

checkExprExistence :: MonadCoherenceCheck m => AnyExprLink -> m ()
checkExprExistence lnk = do
    (_, tgt) <- readLayer @Model lnk
    markExprChecked tgt

checkExprCoherence :: MonadCoherenceCheck m => AnyExpr -> m ()
checkExprCoherence e = do
    tp <- readLayer @Type e
    checkLinkTarget e tp
    mapM_ (checkLinkTarget e) =<< symbolFields e


checkLinkTarget :: MonadCoherenceCheck m => AnyExpr -> AnyExprLink -> m ()
checkLinkTarget e lnk = do
    markLinkChecked lnk
    (_, tgt) <- readLayer @Model lnk
    when (e ^. idx /= tgt ^. idx) $ reportIncoherence (DeteachedSource e lnk)


-- END == WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP WIP ===





data                    SimpleAA
type instance Abstract  SimpleAA = SimpleAA
type instance Inputs    SimpleAA = '[ExprNet, ExprLinkNet, ExprGroupNet] <> ExprLayers '[Model, UID, Type, Succs] <> ExprLinkLayers '[Model, UID]
type instance Outputs   SimpleAA = '[ExprNet, ExprLinkNet, ExprGroupNet] <> ExprLayers '[Model, UID, Type, Succs] <> ExprLinkLayers '[Model, UID]
type instance Events    SimpleAA = '[NEW // EXPR]
type instance Preserves SimpleAA = '[]

pass1 :: (MonadFix m, MonadIO m, IRMonad m, MonadVis m, MonadPassManager m) => Pass SimpleAA m
pass1 = gen_pass1

test_pass1 :: (MonadIO m, MonadFix m, PrimMonad m, MonadVis m, Logging m) => m (Either Pass.InternalError ())
test_pass1 = evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' pass1

test_pass1x :: (MonadIO m, MonadFix m, PrimMonad m, MonadVis m, Logging m, Pass.KnownDescription pass, Pass.PassInit pass (PassManager (IRBuilder m)))
            => Pass pass (PassManager (IRBuilder m)) -> m (Either Pass.InternalError ())
test_pass1x p = evalIRBuilder' $ evalPassManager' $ do
    runRegs
    Pass.eval' p

uncheckedDeleteStar :: (IRMonad m, Readable (ExprLayer Type) m, Accessibles m '[ExprLinkNet, ExprNet]) => Expr l -> m ()
uncheckedDeleteStar e = do
    delete =<< readLayer @Type e
    delete e
{-# INLINE uncheckedDeleteStar #-}

uncheckedDeleteStarType :: (IRMonad m, Readable (ExprLayer Type) m, Accessibles m '[ExprLinkNet, ExprNet, ExprLinkLayer Model])
                        => Expr l -> m ()
uncheckedDeleteStarType e = do
    typeLink     <- readLayer @Type e
    (oldStar, _) <- readLayer @Model typeLink
    uncheckedDeleteStar oldStar
    delete typeLink
{-# INLINE uncheckedDeleteStarType #-}







gen_pass1 :: ( MonadIO m, IRMonad m, MonadVis m
             , Accessibles m '[ExprLayer Model, ExprLinkLayer Model, ExprLayer Type, ExprLayer Succs, ExprLinkLayer UID, ExprLayer UID, ExprNet, ExprLinkNet, ExprGroupNet]
             , Emitter m (NEW // EXPR)
             ) => m ()
gen_pass1 = do
    ss <- string "hello"
    (s :: Expr Star) <- star
    tlink   <- readLayer @Type s
    (src,_) <- readLayer @Model tlink
    scss    <- readLayer @Succs src
    print src
    print scss

    i <- readLayer @UID s
    print i

    Vis.snapshot "s1"


    match s $ \case
        Unify l r -> print "ppp"
        Star      -> match s $ \case
            Unify l r -> print "hola"
            Star      -> print "hellox"


    return ()




main :: IO ()
main = do
    runLogging $ runEchoLogger $ runFormatLogger nestedReportedFormatter $ do
        (p, vis) <- Vis.newRunDiffT test_pass1
        case p of
            Left e -> do
                print "* INTERNAL ERROR *"
                print e
            Right _ -> do
                let cfg = ByteString.unpack $ encode $ vis
                -- putStrLn cfg
                -- liftIO $ openBrowser ("http://localhost:8000?cfg=" <> cfg)
                return ()
        print p
    lmain



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
-- d <- readLayer @Type u
-- print d
--
--
-- md <- readAttr @MyData
-- print md
--
-- ts <- exprs
-- print ts
