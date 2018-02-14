{-# LANGUAGE OverloadedStrings #-}

module OCI.IR.Repr.Vis (module OCI.IR.Repr.Vis, module Vis) where

import Luna.Prelude as Prelude
import Data.Maybe   (maybeToList)

import OCI.IR.Repr.Vis.Class as Vis
import OCI.IR.Class
import Luna.IR.Layer.UID
import Luna.IR.Layer.Type
import OCI.IR.Layer.Class
import OCI.IR.Repr.Styles
import OCI.IR
import Luna.IR
import qualified Data.Map as Map


type Snapshot m = (MonadRef m, MonadVis m, Readers Layer '[AnyExpr // UID, AnyExpr // Type, AnyExpr // Model, Link' AnyExpr // UID, Link' AnyExpr // Model] m, Reader Net AnyExpr m)

snapshot :: Snapshot m => Prelude.String -> m ()
snapshot = snapshotWith (const $ return Nothing) (const $ return Nothing)

snapshotWith :: Snapshot m => (SomeExpr -> m (Maybe Text)) -> (SomeExprLink -> m (Maybe Text)) -> Prelude.String -> m ()
snapshotWith nodeLabeler edgeLabeler title = do
    ts  <- exprs
    vss <- mapM (visNodeWith nodeLabeler edgeLabeler) ts
    let vns = fst <$> vss
        ves = join $ snd <$> vss
    Vis.addStep (fromString title) vns ves


visNodeWith :: (MonadRef m, Readers Layer '[AnyExpr // UID, AnyExpr // Type, AnyExpr // Model, Link' AnyExpr // UID, Link' AnyExpr // Model] m)
        => (SomeExpr -> m (Maybe Text)) -> (SomeExprLink -> m (Maybe Text)) -> SomeExpr -> m (Vis.Node, [Vis.Edge])
visNodeWith nodeLabeler edgeLabeler t = do
    let euid = convert $ unwrap t
    tpLink  <- getLayer @Type  t
    let tpUid = convert $ unwrap tpLink
    (tl,tr) <- getLayer @Model tpLink
    let tlUID = convert $ unwrap tl
        trUID = convert $ unwrap tr

    nlabel <- nodeLabeler t

    ins    <- inputs t

    header <- renderStr HeaderOnly <$> reprExpr t
    value  <- matchExpr t $ return . \case
        String    s   -> "'" <> s <> "'"
        Number    n   -> show n
        Acc       t n -> "Acc "  <> show n
        Var       n   -> "Var "  <> show n
        Cons      n _ -> "Cons " <> show n
        FieldLens p   -> "Lens " <> show p
        UnitProxy u _ -> "UnitProxy '" <> show u <> "'"
        _             -> header

    let node   = Vis.Node (fromString value) euid (convert $ t ^. idx) (fromList [fromString header]) nlabel
        tpVis  = if tlUID == trUID then [] else [Vis.Edge (fromString "") tpUid tpUid tlUID trUID (fromList [fromString "type"]) Nothing]
        mkEdge (ei, (i,l,r), lb) = Vis.Edge (fromString "") i i l r (fromList [fromString ei]) lb
        getUIDs e = do
            (l, r) <- getLayer @Model e
            let i    = convert $ unwrap e
                lUID = convert $ unwrap l
                rUID = convert $ unwrap r
            return (i, lUID, rUID)

    uss     <- mapM getUIDs ins
    elabels <- mapM edgeLabeler ins
    let edges = tpVis <> (mkEdge <$> (zip3 (("input" ++) . show <$> [0::Int ..]) uss elabels))
    return (node, edges)



type SnapshotNoType m = (MonadRef m, MonadVis m, Readers Layer '[AnyExpr // UID, AnyExpr // Model, Link' AnyExpr // UID, Link' AnyExpr // Model] m, Reader Net AnyExpr m, Reader Attr WorldExpr m)
snapshotNoType :: SnapshotNoType m => Prelude.String -> m ()
snapshotNoType title = do
    ts  <- exprs
    vss <- mapM visNodeNoType ts
    let vns = fst <$> vss
        ves = join $ snd <$> vss
    Vis.addStep (fromString title) vns ves


visNodeNoType :: (MonadRef m, Readers Layer '[AnyExpr // UID, AnyExpr // Model, Link' AnyExpr // UID, Link' AnyExpr // Model] m, Reader Attr WorldExpr m)
        => SomeExpr -> m (Vis.Node, [Vis.Edge])
visNodeNoType t = do
    unitMap <- readWorldUnitMap
    euid    <- getLayer @UID   t
    ins     <- inputs t

    let lookupUnitName = case filter ((== t) . snd) (Map.assocs unitMap) of
                             ((name, _) : _) -> name
                             []              -> "unnamed"

    header <- renderStr HeaderOnly <$> reprExpr t
    value  <- matchExpr t $ return . \case
        String    s   -> "'" <> s <> "'"
        Number    n   -> show n
        Var       n   -> "Var "  <> show n
        Cons      n _ -> "Cons " <> show n
        Unit      {}  -> "Unit " <> convert lookupUnitName
        FieldLens p   -> "Lens " <> show p
        Acc       _ n -> "Acc "  <> show n
        Invalid   t   -> "Invalid: " <> show t
        UnitProxy u _ -> "UnitProxy '" <> show u <> "'"
        UnresolvedImport _ s -> "UnresolvedImport " <> show s
        UnresolvedImportSrc s -> "UnresolvedImportSrc " <> show s
        _             -> header

    let node   = Vis.Node (fromString value) euid (convert $ t ^. idx) (fromList [fromString header]) Nothing
        mkEdge (ei, (i,l,r)) = Vis.Edge (fromString "") i i l r (fromList [fromString ei]) Nothing
        getUIDs e = do
            i      <- getLayer @UID   e
            (l, r) <- getLayer @Model e
            lUID   <- getLayer @UID   l
            rUID   <- getLayer @UID   r
            return (i, lUID, rUID)

    uss     <- mapM getUIDs ins
    let edges = mkEdge <$> (zip (("input" ++) . show <$> [0::Int ..]) uss)
    return (node, edges)
