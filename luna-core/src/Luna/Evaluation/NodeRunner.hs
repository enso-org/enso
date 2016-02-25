{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoOverloadedStrings       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Evaluation.NodeRunner where

{-import Prologue-}

{-import           Control.Monad.Catch         (MonadMask, catchAll)-}
{-import           Control.Monad               (forM)-}
{-import           Data.Layer.Coat             (uncoat, Coat, Uncoated, Coated)-}
{-import qualified Data.Text.Lazy              as Text-}
{-import           Language.Haskell.Session    (GhcMonad)-}
{-import           GHC.Prim                    (Any)-}
{-import           Data.Variants               (match, case', ANY(..))-}
{-import           Data.Layer                  (Unlayered)-}
{-import           Data.Maybe                  (fromMaybe, catMaybes, maybeToList, isJust, fromJust)-}
{-import qualified Data.Map                    as Map-}
{-import           Data.Map                    (Map)-}

{-import           Luna.Syntax.Builder.Class   (BuilderMonad)-}
{-import qualified Luna.Syntax.Builder.Class   as Builder-}
{-import qualified Luna.Syntax.Builder         as Builder-}
{-import           Luna.Syntax.Repr.Graph      (Graph)-}

{-import qualified Luna.Syntax.Builder           as Builder-}
{-import qualified Luna.Syntax.Builder.Node      as NodeBuilder-}
{-import qualified Luna.Syntax.Builder.Star      as StarBuilder-}
{-import qualified Luna.Syntax.Builder.Symbol    as SymbolBuilder-}
{-import           Luna.Syntax.Repr.Graph        (Ref(..), Node(..), Edge, DoubleArc)-}
{-import           Luna.Syntax.AST.Term          (Var(..), App(..), Blank(..), Accessor(..), Unify(..), Val(..), Cons(..), Native(..), Draft)-}
{-import qualified Luna.Syntax.AST.Arg           as Arg-}
{-import qualified Luna.Syntax.AST.Lit           as Lit-}
{-import           Luna.Syntax.AST.Decl.Function (Function)-}
{-import qualified Luna.Syntax.AST.Decl.Function as Function-}

{-import qualified Luna.Interpreter.Session    as Session-}

{-import qualified Luna.StdLib.Int             as StdLibInt-}
{-import qualified Luna.Syntax.Symbol.Map      as SymbolMap-}
{-import           Luna.Syntax.Symbol.Map      (SymbolMap, SymbolBuilder)-}
{-import qualified Luna.Syntax.Symbol.QualPath as QualPath-}

{-import qualified Luna.Syntax.AST.Typed       as Typed-}
{-import           Luna.Inference.Literals     (assignLiteralTypes)-}
{-import           Luna.Syntax.Network         (Network)-}

{-type GraphType      a = Network Int (Maybe a)-}
{-type GraphBuilder m a = (BuilderMonad (GraphType a) m, SymbolBuilder (GraphType a) m)-}

{-data NodeValue      a = Fun (Function (GraphType a)) | HaskellVal Any String | Error String | NoVal-}

{-type Bindings         = Map (Ref Node) (Ref Node)-}

{-runGraph gr = flip StarBuilder.evalT Nothing-}
            {-. flip Builder.runT gr-}
            {-. flip NodeBuilder.evalT (Ref $ Node (0 :: Int))-}

{-runInterpreterM gr m = fmap fst-}
                     {-. Session.run-}
                     {-. flip SymbolBuilder.evalT m-}
                     {-. runGraph gr-}

{-getNodeValues :: [Ref Node] -> Network Int (Maybe a) -> IO (Map (Ref Node) (NodeValue a))-}
{-getNodeValues refs gr = runInterpreterM gr Map.empty $ do-}
    {-bindings <- prepareBindings refs-}
    {-SymbolMap.loadSymbols (StdLibInt.symbols :: SymbolMap (Network Int (Maybe a)))-}
    {-fmap Map.fromList $ forM refs $ \ref -> do-}
        {-Builder.modify2 $ runIdentity . (flip runGraph $ assignLiteralTypes ref)-}
        {-val <- flip catchAll (\e -> return (Error $ "Something went wrong: " <> (show e))) $ evalNode bindings ref-}
        {-return (ref, val)-}

{-prepareBindings :: GraphBuilder m a => [Ref Node] -> m Bindings-}
{-prepareBindings refs = fmap Map.fromList $ forM refs $ \ref -> do-}
    {-node <- Builder.readRef ref-}
    {-case' (uncoat node) $-}
        {-match $ \(Unify var tgt) -> (,) <$> Builder.follow var <*> Builder.follow tgt-}

{-printIdent :: GraphBuilder m a => Ref Node -> m String-}
{-printIdent ref = do-}
    {-node <- Builder.readRef ref-}
    {-case' (uncoat node) $ match $ \(Lit.String n) -> return (Text.unpack . toText $ n)-}

{-getConsName :: GraphBuilder m a => Ref Node -> m String-}
{-getConsName ref = do-}
    {-node <- Builder.readRef ref-}
    {-case' (uncoat node) $ match $ \(Cons n _) -> Builder.follow n >>= printIdent-}

{-getSelf :: GraphBuilder m a => Ref Node -> m (Maybe (Ref Node))-}
{-getSelf ref = do-}
    {-node <- Builder.readRef ref-}
    {-case' (uncoat node) $ do-}
        {-match $ \(Accessor _ t) -> Just <$> Builder.follow t-}
        {-match $ \ANY -> return Nothing-}

{-getVal :: Val a -> NodeValue b-}
{-getVal val = case' val $ do-}
    {-match $ \lit -> case lit of-}
        {-Lit.Int i    -> HaskellVal (Session.toAny i) "Int"-}
        {-Lit.String s -> HaskellVal (Session.toAny s) "String"-}

{-resolveSymbol :: GraphBuilder m a => String -> m (NodeValue a)-}
{-resolveSymbol name = do-}
    {-fun <- SymbolMap.lookupSymbol $ QualPath.mk name-}
    {-return $ maybe (Error $ "Can't resolve symbol: " <> name) Fun fun-}

{-callFunction :: NodeValue a -> Maybe (Ref Node) -> [Ref Node] -> GraphType a -> Bindings -> SymbolMap (Network Int (Maybe a)) -> IO (NodeValue a)-}
{-callFunction fun self args graph bindings symbols = case fun of-}
    {-HaskellVal _ _ -> return $ Error "Expected a function, got native value."-}
    {-Error _        -> return NoVal-}
    {-NoVal          -> return NoVal-}
    {-Fun f          -> if isJust self /= isJust (f ^. Function.self) || length args /= length (f ^. Function.args)-}
        {-then return $ Error "Unexpected number of arguments in a function call."-}
        {-else runInterpreterM graph symbols $ do-}
            {-nodeTranslations <- Builder.merge $ f ^. Function.body-}
            {-let unsafeTrans = fromJust . flip Map.lookup nodeTranslations -- It's actually safe, we're in big trouble if a value isn't present in the map.-}
                {-transArgs   = unsafeTrans <$> f ^. Function.args-}
                {-transSelf   = unsafeTrans <$> f ^. Function.self-}
                {-transOut    = unsafeTrans $ f ^. Function.out-}
                {-selfBind    = maybeToList $ (,) <$> transSelf <*> self-}
                {-argBinds    = zip transArgs args-}
                {-newBindings = Map.union bindings (Map.fromList $ selfBind ++ argBinds)-}
            {-evalNode newBindings transOut-}

{-evalNode :: (GhcMonad m, MonadMask m, GraphBuilder m a) => Bindings -> Ref Node -> m (NodeValue a)-}
{-evalNode bindings ref = maybe (runNode bindings ref) (evalNode bindings) $ Map.lookup ref bindings-}

{-valToNative :: NodeValue a -> Maybe (Any, String)-}
{-valToNative (HaskellVal v t) = Just (v, t)-}
{-valToNative _ = Nothing-}

{-runNode :: (GhcMonad m, MonadMask m, GraphBuilder m a) => Bindings -> Ref Node -> m (NodeValue a)-}
{-runNode bindings ref = do-}
    {-node <- Builder.readRef ref-}
    {-case' (uncoat node) $ do-}
        {-match $ \(Accessor n t) -> do-}
            {-name    <- Builder.follow n >>= printIdent-}
            {-selfVal <- Builder.follow t >>= evalNode bindings-}
            {-case selfVal of-}
                {-HaskellVal _ tpName -> do-}
                    {-let funName = tpName <> "." <> name-}
                    {-resolveSymbol funName-}
                {-Error err -> return NoVal-}
                {-_ -> return $ NoVal-}
        {-match $ \(Var n) -> do-}
            {-name <- Builder.follow n >>= printIdent-}
            {-resolveSymbol name-}
        {-match $ \(App f args') -> do-}
            {-fun     <- evalNode bindings =<< Builder.follow f-}
            {-self    <- getSelf =<< Builder.follow f-}
            {-args    <- mapM (Builder.follow . Arg.__arec) args'-}
            {-graph   <- Builder.get-}
            {-symbols <- SymbolBuilder.get-}
            {-res   <- liftIO $ callFunction fun self args graph bindings symbols-}
            {-return res-}
        {-match $ \(Native n args) -> do-}
            {-forcedArgs <- mapM (Builder.follow . Arg.__arec >=> evalNode bindings) args-}
            {-outType    <- Builder.follow (node ^. Typed.tp) >>= getConsName-}
            {-name       <- Builder.follow n >>= printIdent-}
            {-let vals = mapM valToNative forcedArgs-}
            {-case vals of-}
                {-Nothing     -> return $ Error "Can't evaluate native call arguments"-}
                {-Just values -> do-}
                    {-let tpString = (intercalate " -> " $ snd <$> values) <> " -> " <> outType-}
                    {-fun <- Session.findSymbol name tpString-}
                    {-let res = foldl Session.appArg fun $ fst <$> values-}
                    {-return $ HaskellVal res outType-}
        {-match $ \(Unify l r) -> Builder.follow r >>= evalNode bindings-}
        {-match $ return . getVal-}
        {-match $ \ANY -> return $ Error "Unrecognised node type."-}
