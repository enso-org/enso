{-# LANGUAGE ViewPatterns #-}

module OCI.IR.Combinators where

import           Luna.Prelude
import qualified OCI.Pass    as Pass
import           OCI.IR
import           Luna.IR
import           Data.TypeDesc
import qualified Data.Set     as Set
import           Data.List    (nub)

toSomeExpr :: Expr t -> SomeExpr
toSomeExpr = generalize

match2 :: (MonadRef m, Reader Layer (AnyExpr // Model) m, uniTerm ~ Unwrapped (UniTerm' (Expr layout)))
       => Expr layout -> Expr layout -> (uniTerm -> uniTerm -> m a) -> m a
match2 e1 e2 l = do
    firstMatched <- matchExpr e1 $ return . l
    matchExpr e2 firstMatched

{-# DEPRECATED narrowTerm "Use `narrow` instead" #-}
narrowTerm :: forall a m t. (MonadRef m, KnownType (TermTypeOf a), Reader Layer (AnyExpr // Model) m) => Expr t -> m (Maybe (Expr (TermTypeOf a)))
narrowTerm (toSomeExpr -> expr) = flip justIf (unsafeGeneralize expr) . (getTermDesc @a ==) <$> termTermDesc expr; {-# INLINE narrowTerm #-}

-- FIXME [WD]: we can implement it with much better performance by comparing just model's store bits
narrow :: forall a m t. (MonadRef m, KnownType (TermTypeOf a), Reader Layer (AnyExpr // Model) m) => Expr t -> m (Maybe (Expr a))
narrow (toSomeExpr -> expr) = flip justIf (unsafeGeneralize expr) . (getTermDesc @a ==) <$> termTermDesc expr


deleteSubtree :: Emitter (OnDeepDelete // AnyExpr) m => Expr t -> m ()
deleteSubtree = deepDelete


type DelNoInputsCtx m = Req m '[ Editor  // Net   // '[AnyExpr, AnyExprLink]
                               , Editor  // Layer // '[AnyExpr // Succs, AnyExpr // Type, AnyExpr // Model, AnyExprLink // Model]
                               , Emitter // '[OnDeepDelete // AnyExpr, Delete // AnyExpr, Delete // AnyExprLink]
                               ]

deleteWithoutInputs :: DelNoInputsCtx m => Expr t -> m ()
deleteWithoutInputs (toSomeExpr -> expr) = do
    removable <- isSafeToRemove expr
    when removable $ do
        tp  <- getLayer @Type   expr >>= readSource
        delete expr
        deleteSubtree tp

type LinkSrcReplaceCtx m = Req m '[ Editor // Net   // AnyExprLink
                                  , Editor // Layer // '[AnyExpr // Succs, AnyExprLink // Model]
                                  ]

type LinkSrcReplaceCtx' m = Req m '[ Editor // Net // '[AnyExpr, AnyExprLink]
                                   , Editor // Layer // '[AnyExpr // Succs, AnyExprLink // Model]
                                   ]

replaceSource :: ( Generalizable' (Expr a) (Expr a) -- FIXME[WD]: This constrain is ugly as hell and should not be needed
                 , LinkSrcReplaceCtx m) => Expr a -> ExprLink a b -> m ()
replaceSource newSource link = do
    (src, tgt) <- getLayer @Model link
    modifyLayer_ @Succs src       $ Set.delete (generalize link)
    modifyLayer_ @Succs newSource $ Set.insert (generalize link)
    putLayer     @Model link      $ (newSource, tgt)

substitute :: LinkSrcReplaceCtx m => Expr new -> Expr old -> m ()
substitute (toSomeExpr -> new) (toSomeExpr -> old) = mapM_ (replaceSource new) =<< getLayer @Succs old

replace :: (LinkSrcReplaceCtx m, Emitter (OnDeepDelete // AnyExpr) m) => Expr new -> Expr old -> m ()
replace new old = substitute new old >> deleteSubtree old

replace' :: (LinkSrcReplaceCtx' m, Emitter (Delete // AnyExpr) m) => Expr new -> Expr old -> m ()
replace' new old = substitute new old >> delete old

type LayerReconnectCtx l m = (Req m '[ Editor  // Net   // AnyExprLink
                                    -- , Editor  // Layer // AnyExpr // l -- FIXME [WD]: we don't allow for parametrized elems in Req now!
                                    , Emitter // '[Delete // AnyExprLink, New // AnyExprLink]
                                    ], Editors Layer '[AnyExpr // l] m)

reconnectLayer :: forall l m a b b'. (LayerReconnectCtx l m, LayerData l (Expr a) ~ ExprLink b a
                                     , Generalizable (Expr b') (Expr b) ~ 'True) -- FIXME[WD]: This constrain is ugly as hell and should not be needed
               => Expr b' -> Expr a -> m ()
reconnectLayer src tgt = do
    old  <- getLayer @l tgt
    delete old
    link <- link (generalize src) tgt
    putLayer @l tgt link

-- FIXME [WD-MK]: name of the function does not tell anything about how it differs from reconnectLayer
reconnectLayer' :: forall l m a b b' t. (LayerReconnectCtx l m, Traversable t, LayerData l (Expr a) ~  t (ExprLink b a)
                                        , Generalizable' (Expr b') (Expr b)) -- FIXME[WD]: This constrain is ugly as hell and should not be needed
                => t (Expr b') -> Expr a -> m ()
reconnectLayer' srcs tgt = do
    old  <- getLayer @l tgt
    mapM delete old
    links <- forM srcs $ \src -> link (generalize src) tgt
    putLayer @l tgt links
