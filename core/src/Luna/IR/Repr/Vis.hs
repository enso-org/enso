module Luna.IR.Repr.Vis (module Luna.IR.Repr.Vis, module Vis) where

import Luna.Prelude as Prelude

import Luna.IR.Repr.Vis.Class as Vis
import Luna.IR.Internal.IR
import Luna.IR.Layer.UID
import Luna.IR.Layer.Type
import Luna.IR.Layer.Model
import Luna.IR.Repr.Styles
import Luna.IR


snapshot :: (MonadRef m, MonadVis m, Editors Layer '[AnyExpr // UID, AnyExpr // Type, AnyExpr // Model, Link' AnyExpr // UID, Link' AnyExpr // Model] m, Editor Net AnyExpr m)
         => Prelude.String -> m ()
snapshot title = do
    ts  <- exprs
    vss <- mapM visNode2 ts
    let vns = fst <$> vss
        ves = join $ snd <$> vss
    Vis.addStep (fromString title) vns ves


visNode2 :: (MonadRef m, Editors Layer '[AnyExpr // UID, AnyExpr // Type, AnyExpr // Model, Link' AnyExpr // UID, Link' AnyExpr // Model] m)
         => SomeExpr -> m (Vis.Node, [Vis.Edge])
visNode2 t = do
    euid   <- readLayer @UID   t
    tpLink <- readLayer @Type  t
    tpUid  <- readLayer @UID   tpLink
    (l,r)  <- readLayer @Model tpLink
    lUID   <- readLayer @UID   l
    rUID   <- readLayer @UID   r
    ins    <- symbolFields t

    header <- renderStr HeaderOnly <$> reprExpr t
    value  <- match t $ return . \case
        String s -> "'" <> s <> "'"
        _        -> header

    -- let node   = Vis.Node (fromString value) euid euid (fromList [header])
    let node   = Vis.Node (fromString value) euid euid (fromList [fromString header])
        tpVis  = if lUID == rUID then [] else [Vis.Edge (fromString "") tpUid tpUid lUID rUID (fromList [fromString "type"])]
        mkEdge (i,l,r) = Vis.Edge (fromString "") i i l r mempty
        getUIDs e = do
            i      <- readLayer @UID   e
            (l, r) <- readLayer @Model e
            lUID   <- readLayer @UID   l
            rUID   <- readLayer @UID   r
            return (i, lUID, rUID)

    uss <- mapM getUIDs ins

    let edges = tpVis <> (mkEdge <$> uss)
    return (node, edges)
