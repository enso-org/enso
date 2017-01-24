{-# LANGUAGE ViewPatterns #-}

module Luna.IR.Expr.Combinators where

import           Luna.Prelude
import qualified Luna.Pass    as Pass
import           Luna.IR
import           Data.TypeDesc
import qualified Data.Set     as Set

toSomeExpr :: Generalizable' t Bottom => Expr t -> SomeExpr
toSomeExpr = generalize

narrowAtom :: forall a m. (MonadRef m, KnownType (AtomOf a), Reader Layer (AnyExpr // Model) m)
           => SomeExpr -> m (Maybe (Expr (AtomOf a)))
narrowAtom expr = fromBoolMaybe (unsafeGeneralize expr) . (getAtomDesc @a ==) <$> termAtomDesc expr ; {-# INLINE narrowAtom #-}

safeToRemove :: forall t m. (MonadRef m, Readers Net '[AnyExpr, AnyExprLink] m,
                           Readers Layer '[AnyExpr // Succs, AnyExpr // Model, AnyExprLink // Model] m, Generalizable' t Bottom)
             => Expr t -> m Bool
safeToRemove (toSomeExpr -> expr) = do
    succs     <- readLayer @Succs expr
    case Set.toList succs of
        []  -> return True
        [l] -> uncurry (==) <$> readLayer @Model l -- Detect a loop, occuring e.g. with Stars
        _   -> return False

deleteSubtree :: forall t m. (MonadRef m, Editors Net '[AnyExpr, AnyExprLink] m, Editors Layer '[AnyExpr // Succs, AnyExpr // Type, AnyExpr // Model, AnyExprLink // Model] m, Emitters '[Delete // AnyExpr, Delete // AnyExprLink] m, Generalizable' t Bottom)
                 => Expr t -> m ()
deleteSubtree (toSomeExpr -> expr) = do
    removable <- safeToRemove expr
    when removable $ do
        inps       <- symbolFields expr
        tp         <- readLayer @Type expr
        toRemove   <- mapM source $ tp : inps
        delete expr
        mapM_ deleteSubtree $ filter (/= expr) toRemove

deleteWithoutInputs :: forall t m. (MonadRef m, Editors Net '[AnyExpr, AnyExprLink] m,
                                  Editors Layer '[AnyExpr // Succs, AnyExpr // Type,
                                                  AnyExpr // Model, AnyExprLink // Model] m,
                                  Emitters '[Delete // AnyExpr, Delete // AnyExprLink] m, Generalizable' t Bottom)
                 => Expr t -> m ()
deleteWithoutInputs (toSomeExpr -> expr) = do
    removable <- safeToRemove expr
    when removable $ do
        tp         <- readLayer @Type expr
        toRemove   <- source tp
        delete expr
        deleteSubtree toRemove

changeSource :: forall a b m. (MonadRef m, Editors Net '[AnyExprLink] m, Editors Layer '[AnyExpr // Succs, AnyExprLink // Model] m, Generalizable' (Expr a) (Expr a))
             => ExprLink a b -> Expr a -> m ()
changeSource link newSource = do
    (src, tgt) <- readLayer @Model link
    modifyLayer_ @Succs (Set.delete $ generalize link) src
    modifyLayer_ @Succs (Set.insert $ generalize link) newSource
    writeLayer   @Model (newSource, tgt) link

replaceNode :: forall a b m. (MonadRef m, Editors Net '[AnyExprLink] m, Editors Layer '[AnyExpr // Succs, AnyExprLink // Model] m, Generalizable' a Bottom, Generalizable' b Bottom)
            => Expr a -> Expr b -> m ()
replaceNode (toSomeExpr -> old) (toSomeExpr -> new) = do
    succs <- readLayer @Succs old
    mapM_ (flip changeSource new) $ Set.toList succs

reconnectLayer :: forall l m a b b'. (MonadRef m, Editors Net '[AnyExprLink] m, Editors Layer '[AnyExpr // l] m, Emitters '[Delete // AnyExprLink, New // AnyExprLink] m, LayerData l (Expr a) ~ ExprLink b a, Generalizable (Expr b') (Expr b) ~ 'True)
               => Expr b' -> Expr a -> m ()
reconnectLayer src tgt = do
    old  <- readLayer @l tgt
    delete old
    link <- link (generalize src) tgt
    writeLayer @l link tgt

reconnectLayer' :: forall l m a b b' t. (MonadRef m, Editors Net '[AnyExprLink] m, Editors Layer '[AnyExpr // l] m, Emitters '[Delete // AnyExprLink, New // AnyExprLink] m, Traversable t, LayerData l (Expr a) ~  t (ExprLink b a), Generalizable' (Expr b') (Expr b))
                => t (Expr b') -> Expr a -> m ()
reconnectLayer' srcs tgt = do
    old  <- readLayer @l tgt
    mapM delete old
    links <- forM srcs $ \src -> link (generalize src) tgt
    writeLayer @l links tgt
