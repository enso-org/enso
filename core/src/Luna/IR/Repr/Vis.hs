module Luna.IR.Repr.Vis (module Luna.IR.Repr.Vis, module Vis) where

import Luna.Prelude as Prelude

import Luna.IR.Repr.Vis.Class as Vis
import Luna.IR.Internal.IR
import Luna.IR.Layer.UID
import Luna.IR.Layer.Type
import Luna.IR.Layer.Model
import Luna.IR.Repr.Styles
import Luna.IR


snapshot :: (IRMonad m, MonadVis m, Readables m '[ExprLayer UID, ExprLayer Type, ExprLayer Model, ExprLinkLayer UID, ExprLinkLayer Model, ExprNet])
         => Prelude.String -> m ()
snapshot title = do
    ts  <- exprs
    vss <- mapM visNode2 ts
    let vns = fst <$> vss
        ves = join $ snd <$> vss
    Vis.addStep (fromString title) vns ves


visNode2 :: (IRMonad m, Readables m '[ExprLayer UID, ExprLayer Type, ExprLayer Model, ExprLinkLayer UID, ExprLinkLayer Model])
         => Expr' -> m (Vis.Node, [Vis.Edge])
visNode2 t = do
    euid   <- readLayer @UID   t
    tpLink <- readLayer @Type  t
    tpUid  <- readLayer @UID   tpLink
    (l,r)  <- readLayer @Model tpLink
    lUID   <- readLayer @UID   l
    rUID   <- readLayer @UID   r
    ins    <- symbolFields t

    header <- fromString . renderStr HeaderOnly <$> reprExpr t
    value  <- match t $ return . \case
        String s -> "'" <> s <> "'"
        _        -> ""

    let node   = Vis.Node (fromString value) euid euid (fromList [header])
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
