module Luna.IR.Expr.Combinators where

import           Luna.Prelude
import qualified Luna.Pass    as Pass
import           Luna.IR
import           Data.TypeDesc
import qualified Data.Set     as Set

narrowAtom :: forall a m. (MonadRef m, KnownType (AtomOf a), Reader Layer (AnyExpr // Model) m)
           => SomeExpr -> m (Maybe (Expr (AtomOf a)))
narrowAtom expr = fromBoolMaybe (unsafeGeneralize expr) . (getAtomDesc @a ==) <$> termAtomDesc expr ; {-# INLINE narrowAtom #-}

deleteSubtree :: forall l m. (MonadRef m, Editors Net '[AnyExpr, AnyExprLink] m, Editors Layer '[AnyExpr // Succs, AnyExpr // Type, AnyExpr // Model, AnyExprLink // Model] m, Emitters '[Delete // AnyExpr, Delete // AnyExprLink] m)
                 => SomeExpr -> m ()
deleteSubtree expr = do
    succs     <- readLayer @Succs expr
    removable <- case Set.toList succs of
        []  -> return True
        [l] -> uncurry (==) <$> readLayer @Model l -- Detect a loop, occuring e.g. with Stars
        _   -> return False
    if removable then do
        inps       <- symbolFields expr
        tp         <- readLayer @Type expr
        toRemove   <- mapM source $ tp : inps
        delete expr
        mapM_ deleteSubtree $ filter (/= expr) toRemove
    else return ()

changeSource :: forall l m. (MonadRef m, Editors Net '[AnyExprLink] m, Editors Layer '[AnyExpr // Succs, AnyExprLink // Model] m)
             => SomeExprLink -> SomeExpr -> m ()
changeSource link newSource = do
    (src, tgt) <- readLayer @Model link
    modifyLayer_ @Succs (Set.delete link) src
    modifyLayer_ @Succs (Set.insert link) newSource
    writeLayer   @Model (newSource, tgt) link

replaceNode :: forall l m. (MonadRef m, Editors Net '[AnyExprLink] m, Editors Layer '[AnyExpr // Succs, AnyExprLink // Model] m)
            => SomeExpr -> SomeExpr -> m ()
replaceNode old new = do
    succs <- readLayer @Succs old
    mapM_ (flip changeSource new) $ Set.toList succs
