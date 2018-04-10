{-# LANGUAGE OverloadedStrings #-}

module Luna.Pass.Evaluation.Interpreter where

import Prelude as P (read)
import Luna.Prelude   as P hiding (seq, force, Constructor, Text)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.IORef    (IORef, newIORef, readIORef, writeIORef)

import Data.Text.Lazy (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (numerator)
import Data.Maybe (fromMaybe)
import Luna.IR.Term.Literal (isInteger, toDouble, toInt)
import qualified Luna.IR.Layer.Errors as Errors

import           Luna.IR   hiding (get, put, modify)
import           OCI.Pass (SubPass, Inputs, Outputs, Preserves, Events)
import qualified OCI.Pass        as Pass
import qualified Control.Monad.Trans.State.Strict as State

import Luna.Builtin.Data.LunaValue
import Luna.Builtin.Prim
import Luna.Builtin.Data.Module    as Module
import Luna.Builtin.Data.Function  as Function
import Luna.Builtin.Data.LunaEff    (runError, throw, runIO, performIO, LunaEff)

data Interpreter
type instance Abstract Interpreter = Interpreter
type instance Inputs  Net   Interpreter = '[AnyExpr, AnyExprLink]
type instance Outputs Net   Interpreter = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer Interpreter = '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model, AnyExpr // Errors]
type instance Outputs Layer Interpreter = '[]
type instance Inputs  Attr  Interpreter = '[]
type instance Outputs Attr  Interpreter = '[]
type instance Inputs  Event Interpreter = '[]
type instance Outputs Event Interpreter = '[]
type instance Preserves     Interpreter = '[]

------------------------
-- === LocalScope === --
------------------------

data LocalScope  = LocalScope { _localVars   :: Map (Expr Draft) LunaData
                              , _localDefs   :: Map Name LunaValue
                              }
makeLenses ''LocalScope

instance Default LocalScope where
    def = LocalScope def def

localLookup :: Expr Draft -> LocalScope -> Maybe LunaData
localLookup e = Map.lookup e . view localVars

localDefLookup :: Name -> LocalScope -> Maybe LunaValue
localDefLookup e = Map.lookup e . view localDefs

localInsert :: Expr Draft -> LunaData -> LocalScope -> LocalScope
localInsert e d = localVars %~ Map.insert e d

mergeScopes :: Map (Expr Draft) LunaData -> LocalScope -> LocalScope
mergeScopes m = localVars %~ Map.union m

globalLookup :: Name -> Imports -> Maybe LunaValue
globalLookup n imps = imps ^? importedFunctions . ix n . documentedItem . _Right . value

------------------------
-- === MonadScope === --
------------------------

newtype ScopeT m a = ScopeT
        { unScopeT :: State.StateT (IORef LocalScope) m a
        } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)


class Monad m => MonadScope m where
    get :: m LocalScope
    put :: LocalScope -> m ()

instance MonadIO m => MonadScope (ScopeT m) where
    get   = ScopeT $ State.get >>= liftIO . readIORef
    put s = ScopeT $ State.get >>= liftIO . flip writeIORef s

modify :: MonadScope m => (LocalScope -> LocalScope) -> m ()
modify f = fmap f get >>= put

gets :: MonadScope m => (LocalScope -> a) -> m a
gets f = f <$> get

runScopeT :: MonadIO m => ScopeT m a -> LocalScope -> m (a, LocalScope)
runScopeT m scope = do
    ref      <- liftIO $ newIORef scope
    result   <- State.evalStateT (unScopeT m) ref
    newState <- liftIO $ readIORef ref
    return (result, newState)

evalScopeT :: MonadIO m => ScopeT m a -> LocalScope -> m a
evalScopeT m scope = fst <$> runScopeT m scope

------------------------------------
-- === Constants construction === --
------------------------------------

mkInt :: Imports -> Integer -> LunaData
mkInt = toLunaData

mkDouble :: Imports -> Double -> LunaData
mkDouble = toLunaData

mkString :: Imports -> Text -> LunaData
mkString = toLunaData

mkNothing :: Imports -> LunaData
mkNothing imps = toLunaData imps ()

----------------------------
-- === Interpretation === --
----------------------------

interpret :: MonadRef m => Imports -> Expr Draft
          -> SubPass Interpreter m (ScopeT LunaEff LunaData)
interpret = interpret'

interpret' :: ( MonadRef m
              , Readers Layer '[ AnyExpr // Model
                               , AnyExpr // Type
                               , AnyExprLink // Model
                               , AnyExpr // Errors
                               ] m
              , Editors Net '[AnyExpr, AnyExprLink] m)
           => Imports -> Expr Draft -> m (ScopeT LunaEff LunaData)
interpret' glob expr = do
    errors    <- getLayer @Errors expr
    hasErrors <- not . null <$> getLayer @Errors expr
    let errorValue = return $ LunaError (head errors ^. Errors.description . to convert)
    if hasErrors then return errorValue else matchExpr expr $ \case
        String s  -> let res = mkString glob (convert s) in return $ return res
        Number a  -> let res = if isInteger a then mkInt glob $ toInt a else mkDouble glob $ toDouble a in return $ return res
        Var name  -> do
            let globSym = globalLookup name glob
            return $ do
                localDef <- gets (localDefLookup name)
                local    <- gets (localLookup expr)
                case (local, localDef, globSym) of
                    (Just v, _, _) -> return v
                    (_, Just v, _) -> lift $ v
                    (_, _, Just v) -> lift $ v
                    _              -> return LunaNoValue
        Lam i' o' -> do
            bodyVal    <- interpret' glob    =<< source o'
            inpMatcher <- irrefutableMatcher =<< source i'
            return $ do
                env <- get
                return $ LunaFunction $ \d -> do
                    newBinds <- inpMatcher $ LunaThunk d
                    evalScopeT bodyVal $ mergeScopes newBinds env
        App f' a' -> do
            f   <- source f'
            a   <- source a'
            fun <- interpret' glob f
            arg <- interpret' glob a
            return $ do
                env  <- get
                let fun' = evalScopeT fun env
                    arg' = evalScopeT arg env
                lift $ force $ applyFun fun' arg'

        Acc a' name -> do
            a <- source a' >>= interpret' glob
            return $ do
                arg <- a
                lift $ force $ dispatchMethod name arg
        Unify l' r' -> do
            l    <- source l'
            r    <- source r'
            rhs  <- interpret' glob r
            lpat <- irrefutableMatcher l
            return $ do
                env  <- get
                let rhs' = evalScopeT rhs (localInsert l (LunaThunk rhs') env)
                rhsV <- lift $ runError $ force rhs'
                case rhsV of
                    Left e -> do
                        modify $ localInsert l $ LunaError e
                        lift $ throw e
                    Right v -> lift (lpat v) >>= modify . mergeScopes
                return $ mkNothing glob
        ASGFunction n' as' b' -> do
            n   <- source n'
            as  <- mapM (irrefutableMatcher <=< source) as'
            rhs <- interpret' glob =<< source b'
            let makeFuns []       e = evalScopeT rhs e
                makeFuns (m : ms) e = return $ LunaFunction $ \d -> do
                    newBinds <- m $ LunaThunk d
                    makeFuns ms (mergeScopes newBinds e)
            return $ do
                env <- get
                let rhs' = makeFuns as (localInsert n (LunaThunk rhs') env)
                modify $ localInsert n (LunaThunk rhs')
                return $ LunaSusp rhs'
        Seq l' r'  -> do
            l   <- source l'
            r   <- source r'
            lhs <- interpret' glob l
            rhs <- interpret' glob r
            return $ do
                lV <- lhs
                lift $ forceThunks' lV
                rhs
        Cons n fs -> do
            fields <- mapM (interpret' glob <=< source) fs
            let mets = getConstructorMethodMap n glob
            return $ do
                fs        <- sequence fields
                return $ LunaObject $ Object (Constructor n fs) mets
        Match t' cls' -> do
            t       <- source t'
            cls     <- mapM source cls'
            target  <- interpret' glob t
            clauses <- forM cls $ \clause -> matchExpr clause $ \case
                Lam pat res -> (,) <$> (matcher =<< source pat)
                                   <*> (interpret' glob =<< source res)
            return $ do
                env <- get
                let tgt' = evalScopeT target env
                tgt <- lift $ force tgt'
                (scope, cl) <- lift $ runMatch clauses tgt
                lift $ evalScopeT cl (mergeScopes scope env)
        Marked _ b -> interpret' glob =<< source b
        s -> error $ "unexpected " ++ show s

type MatchRes  = (Maybe (Map (Expr Draft) LunaData), LunaData)
type MatchResM = LunaEff MatchRes
type Matcher   = LunaData -> MatchResM

runMatch :: [(Matcher, a)] -> LunaData -> LunaEff (Map (Expr Draft) LunaData, a)
runMatch [] _ = throw "Inexhaustive Luna pattern match"
runMatch ((m, p) : ms) d = do
    (res, nextObj) <- m d
    case res of
        Just newScope -> return (newScope, p)
        Nothing       -> runMatch ms nextObj

tryConsMatch :: Name -> [Matcher] -> Matcher
tryConsMatch name fieldMatchers d' = force' d' >>= go where
    go d@(LunaObject (Object (Constructor n fs) ms)) = if n == name then matchFields else return (Nothing, d) where
        matchFields :: MatchResM
        matchFields = do
            results <- zipWithM ($) fieldMatchers fs
            let binds  = fmap Map.unions $ sequence $ fst <$> results
                newObj = LunaObject (Object (Constructor n (snd <$> results)) ms)
            return (binds, newObj)
    go d = return (Nothing, d)

tryBoxedMatch :: (Eq a, IsBoxed a) => a -> Matcher
tryBoxedMatch i d' = force' d' >>= go where
    go d@(LunaBoxed o) = return (if fromBoxed o == i then Just def else Nothing, d)
    go d = return (Nothing, d)

matcher :: (MonadRef m, Readers Layer '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model] m, Editors Net '[AnyExpr, AnyExprLink] m)
        => Expr Draft -> m Matcher
matcher expr = matchExpr expr $ \case
    Var  _    -> return $ \d -> return (Just $ Map.fromList [(expr, d)], d)
    Grouped g -> matcher =<< source g
    Cons n as -> do
        argMatchers <- mapM (matcher <=< source) as
        return $ tryConsMatch n argMatchers
    Number n -> if isInteger n then return $ tryBoxedMatch $ toInt n else return $ tryBoxedMatch $ toDouble n
    String s -> return $ tryBoxedMatch $ (convert s :: Text)
    Blank -> return $ \d -> return (Just def, d)
    s -> error $ "unexpected pattern: " ++ show s

irrefutableMatcher :: (MonadRef m, Readers Layer '[AnyExpr // Model, AnyExpr // Type, AnyExprLink // Model] m, Editors Net '[AnyExpr, AnyExprLink] m)
                   => Expr Draft -> m (LunaData -> LunaEff (Map (Expr Draft) LunaData))
irrefutableMatcher expr = do
    m <- matcher expr
    return $ \d -> do
        res <- fst <$> m d
        maybe (throw "Irrefutable pattern match failed.") return res
