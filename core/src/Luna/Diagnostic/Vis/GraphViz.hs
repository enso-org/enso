{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# LANGUAGE UndecidableInstances      #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

module Luna.Diagnostic.Vis.GraphViz where

import           Prelude.Luna                            hiding (index)

import           Data.Container
import           Data.Container.Class
import           Data.Index                              (idx)
import           Data.Layer.Cover                        (uncover)
import           Data.List                               (find)
import           Data.Map.Strict                         (Map)
import qualified Data.Map.Strict                         as Map
import           Data.Maybe                              (maybe, maybeToList, fromMaybe)
import           Data.Prop
import           Data.Record
import           Data.Reprx

import           Data.Graph.Backend.VectorGraph          hiding (subGraphs)
import qualified Data.Graph.Backend.VectorGraph          as Graph
import qualified Data.Graph.Backend.VectorGraph.SubGraph as SubGraph
import           Data.GraphViz
import qualified Data.GraphViz.Attributes                as GV
import qualified Data.GraphViz.Attributes.Colors         as GVC
import qualified Data.GraphViz.Attributes.Colors.X11     as GVC
import           Data.GraphViz.Attributes.Complete       hiding (Int, Label, Star, focus)
import qualified Data.GraphViz.Attributes.Complete       as GV
import qualified Data.GraphViz.Attributes.HTML           as Html
import           Data.GraphViz.Commands
import           Data.GraphViz.Printing                  (renderDot, toDot)
import           Data.GraphViz.Printing                  (PrintDot)
import           Data.GraphViz.Types.Canonical

import           Luna.Compilation.Pass.Interpreter.Layer (InterpreterData (..))
import qualified Luna.Compilation.Pass.Interpreter.Layer as InterpreterLayer
import           Luna.Evaluation.Runtime                 (Dynamic, Static)
import qualified Luna.Syntax.AST.Function                as Function
import qualified Luna.Syntax.AST.Term                    as Term
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder
import           Luna.Syntax.Model.Network.Builder.Layer
import           Luna.Syntax.Model.Network.Builder.Term  hiding (match)
import           Luna.Syntax.Model.Network.Term
import           Luna.Syntax.Repr.Styles                 (HeaderOnly (..), Simple (..))

import           System.Platform
import           System.Process                          (createProcess, shell)
import           Data.Tuple.Select                       (sel1)
import qualified Luna.Syntax.AST.Term.Lit                as Lit


--instance Repr HeaderOnly Data where repr _ = "Data"
--instance Repr HeaderOnly (Draft l v) where repr _ = "Draft"

-- Skin definition

bgClr         = GVC.Gray12
gClr          = GVC.Gray30

typedArrClr   = GVC.Firebrick
namedArrClr   = GVC.Turquoise
redirectClr   = GVC.LightPink
accArrClr     = GVC.Yellow
arrClr        = GVC.DarkOrange

idClr         = HSV 0.0 0.0 0.40
starClr       = HSV 0.18 0.64 0.8
nodeClr       = HSV 0.58 0.64 0.8
valIntNodeClr = HSV 0.2 0.64 0.8
valStrNodeClr = HSV 0.3 0.6 0.8
valUnkNodeClr = GVC.Red
dirtyClr      = GVC.MediumOrchid
checkedClr    = GVC.MediumOrchid

selfClr       = GVC.White
argClr        = GVC.LightYellow3
outClr        = GVC.LightPink
tpRepClr      = GVC.DarkOrange

graphLabelClr = GVC.Gray30
nodeLabelClr  = GVC.Gray8
unifyLabelClr = GVC.Gray60
edgeLabelClr  = GVC.Gray40

portClr = HSV 0.0 0.0 0.25
portClr2 = HSV 0.0 0.0 1.00

fontName = "arial"
fontSize = 10.0


gStyle :: String -> [GlobalAttributes]
gStyle name = [ GraphAttrs [ RankDir FromTop
                           , Splines SplineEdges
                           , fontColor graphLabelClr
                           , FontName fontName
                           , FontSize fontSize
                           , Compound True
                           , bgColor  bgClr
                           , color    gClr
                           , GV.Label $ StrLabel $ fromString name
                           ]
              , NodeAttrs  [ fontColor nodeLabelClr
                           , FontName fontName
                           , FontSize fontSize
                           , bgColor valUnkNodeClr
                           , fillColor valUnkNodeClr
                           ]
              , EdgeAttrs  [ fontColor edgeLabelClr
                           , FontName fontName
                           , FontSize fontSize
                           ]
              ]



--check :: NetGraph a -> Coherence

toGraphViz :: String -> NetGraph -> DotGraph String
toGraphViz name net = DotGraph { strictGraph     = False
                               , directedGraph   = True
                               , graphID         = Nothing
                               , graphStatements = DotStmts { attrStmts = gStyle name
                                                            , subGraphs = subGraphs
                                                            , nodeStmts = nodeStmts
                                                            , edgeStmts = edgeStmts
                                                            }
                               }
    where -- === Inputs === --

          ng                = net ^. wrapped' ∘ nodeGraph
          eg                = net ^. wrapped' ∘ edgeGraph
          cg                = net ^. wrapped' ∘ Graph.subGraphs
          nodeIxs           = usedIxes ng :: [Int]
          edgeIxs           = usedIxes eg :: [Int]
          clrIxs            = usedIxes cg
          clrs              = elems $ net ^. wrapped' ∘ Graph.subGraphs
          clredNodeIxs      = zip (safeHead ∘ matchClusters clrIxs <$> nodeIxs) nodeIxs :: [(Maybe Int, Int)]
          clredNodeMap      = fromListWithReps clredNodeIxs :: Map (Maybe Int) [Int]
          rootNodeIxs       = case Map.lookup Nothing clredNodeMap of
                                  Nothing  -> []
                                  Just ixs -> ixs


          -- === outputs === --

          inEdges           = concat $ fmap nodeInEdges nodeIxs
          edgeStmts         = fmap mkEdge inEdges
          nodeStmts         = concat $ labeledNode Nothing <$> rootNodeIxs
          subGraphs         = uncurry genSubGraph ∘ (_1 %~ fromJust) <$> Map.assocs (Map.delete Nothing clredNodeMap)


          -- === Utils === --

          inPortName num    = fromString $ "in" <> show num
          nodeRef         i = "<node " <> show i <> ">"

          --labeledNode ix    = DotNode ref attrs where
          --    ref    = nodeRef ix
          --    node   = draftNodeByIx ix
          --    label  = GV.Label ∘ StrLabel ∘ fromString $ genNodeLabel node
          --    colors = nodeColorAttr node
          --    attrs  = label : colors

          isOrphanTgt (node :: Ref Node (NetLayers :<: Draft Static)) (edge :: Ref Edge (Link (NetLayers :<: Draft Static)))
                    = not $ existing && validSource && validTarget where
              existing    = (edge ^. idx) `elem` edgeIxs
              validSource = edge' ^. source == node
              validTarget = edge `elem` (tgt' # Type) : (maybeToList $ tgt' ^. prop TCData . redirect) ++ (tgt' # Inputs)

              edge' = net ^. focus edge :: Link (NetLayers :<: Draft Static)
              tgt   = edge' ^. target
              tgt'  = net ^. focus tgt

          matchOrphanTgt nix e = if isOrphanTgt nix e then Just e else Nothing

          selectOrphanTgts nix  = catMaybes ∘ fmap (matchOrphanTgt nix)


          labeledNode fptr nix = [ DotNode nodeId attrs ]
                               -- <> orphanTgtNodes
              where
              nodeId   = nodeRef nix
              node     = draftNodeByIx nix
              ins      = node # Inputs
              succs    = readSuccs node
              dirty    = if (node # InterpreterData) ^. InterpreterLayer.dirty    then "●" else "○"
              required = if (node # InterpreterData) ^. InterpreterLayer.required then " ⚑" else ""
              time     = (show $ (node # InterpreterData) ^. InterpreterLayer.time) <> "μs "
              -- value    = fromMaybe "" $ (\v -> " Just Any") <$> (node # InterpreterData) ^. InterpreterLayer.value
              value    = (node # InterpreterData) ^. InterpreterLayer.debug
              interpr  = " " <> time <> value <> required <> " "
              succs'   = (net ^.) ∘ focus <$> succs :: [Link (NetLayers :<: Draft Static)]

              orphanTgts = selectOrphanTgts (Ref nix) succs -- FIXME[WD] ugliness

              orphanTgtNodes = flip DotNode [shape PointShape, emptyLabel] ∘ ((nodeId <> "orphanTgt ") <>) ∘ show <$> orphanTgts


              inPortsNum    = length ins
              inPorts       = port        <$> [0 .. inPortsNum - 1]
              phInPorts     = phantomPort <$> [0 .. inPortsNum - 1]
              recInLayout   = if length inPorts < 1 then [] else [Html.Cells $ blankCell : inPorts]
              subInLayout   = if length inPorts < 1 then [] else [Html.Cells $ phInPorts]
              matchInLayout = if length inPorts < 1 then [] else [Html.Cells $ reverse $ phInPorts] -- reversed ports, so the arrow shows right pattern matching direction
              emptyLabel    = GV.Label  ∘ StrLabel ∘ fromString $ ""
              idlabel       = GV.XLabel ∘ StrLabel ∘ fromString ∘ show $ nix
              --htmlCells  = Html.Cells [idCell $ show nix, labelCell width $ fromString $ genNodeLabel node <> show (length orphanTgts)] where

              htmlCells  = Html.Cells [idCell $ show nix, labelCell width $ fromString $ dirty <> genNodeLabel node
                                            <> "(" <> show (length orphanTgts) <> ") "
                                            <> show (toList $ node # Succs)
                                            <> interpr
                                            <> "[" <> show (view idx <$> node ^. prop TCData . belongsTo) <> "]"] where
                  width  = if null inPorts then 1 else fromIntegral inPortsNum


              labelCell cs s  = Html.LabelCell [Html.ColSpan cs, Html.BGColor $ color, Html.Port "label"] $ Html.Text [Html.Str $ fromString s]
              idCell       s  = Html.LabelCell [Html.ColSpan 1 , Html.Border 0, Html.Port "id"]    $ Html.Text [Html.Font [Html.Color idClr] [Html.Str $ fromString s]]
              blankCell       = Html.LabelCell [Html.Border 0] ""
              spacerCell   w  = Html.LabelCell [Html.Border 0, Html.Width w] ""
              port        num = Html.LabelCell [Html.ColSpan 1 , Html.Height 2, Html.Border 0, Html.Port $ inPortName num, Html.BGColor $ portClr] ""
              phantomPort num = Html.LabelCell [Html.ColSpan 1 , Html.Height 0, Html.Width 0, Html.Border 0, Html.Port $ inPortName num] ""

              recordLabel    = GV.Label $ HtmlLabel $ Html.Table $ Html.HTable Nothing [Html.CellSpacing 3, Html.CellBorder 1, Html.Border 0] $ recInLayout <> [htmlCells]
              subLabel       = GV.Label $ HtmlLabel $ Html.Table $ Html.HTable Nothing [Html.CellSpacing 10, Html.CellBorder 1, Html.Border 0] $ subInLayout
              matchLabel     = GV.Label $ HtmlLabel $ Html.Table $ Html.HTable Nothing [Html.CellSpacing 10, Html.CellBorder 1, Html.Border 0] $ matchInLayout
              unifyLabel     = GV.Label $ HtmlLabel $ Html.Table $ Html.HTable Nothing [Html.CellSpacing 3, Html.CellBorder 1, Html.Border 0] $ [cells] where
                               cells = Html.Cells [idCell $ show nix, spacerCell 40]

              starColor      = toColorList [starClr]
              nodeColor      = toColorList [color]
              bgColor'       = toColorList [GVC.X11Color bgClr]
              color          = case fptr >>= getLambdaNodeColor nix of
                  Just c  -> GVC.X11Color c
                  Nothing -> getNodeColor node
              --attrs   = GV.color color : shAttrs
              --attrs   = Color (toColorList [color]) : shAttrs
              attrs = specAttrs
              specAttrs = caseTest (uncover node) $ do
                  {-of' $ \Term.Star        -> [Color bgColor' , shape Star         , emptyLabel  , FixedSize SetNodeSize, Width 0.4, Height 0.4, PenWidth 6, FillColor starColor, Style [SItem Filled []]]-}
                  of' $ \(Term.Unify a b) -> [Color nodeColor, shape DoubleCircle , unifyLabel  , FixedSize SetNodeSize, Width 0.2, Height 0.2, fontColor unifyLabelClr]
                  {-of' $ \(Term.Match a b) -> [Color nodeColor, shape PrimerSite   , matchLabel  , FixedSize SetNodeSize, Width 0.4, Height 0.4, fontColor unifyLabelClr]-}
                  --of' $ \(Term.Sub   a b) -> [Color nodeColor, shape RArrow       , subLabel    , FixedSize SetNodeSize, Width 0.2, Height 0.2, fontColor unifyLabelClr]
                  of' $ \ANY              -> [Color nodeColor, shape PlainText    , recordLabel ]

          nodeInEdges   n   = zip3 ([0..] :: [Int]) (genInEdges net $ (cast $ index n ng :: NetLayers :<: Draft Static)) (repeat n)
          mkEdge  (n,(a,attrs),b) = DotEdge (nodeRef a) (nodeRef b) $ HeadPort (LabelledPort (inPortName n) Nothing) : TailPort (LabelledPort "label" Nothing) : attrs

          draftNodeByIx ix   = cast $ index_ ix ng :: (NetLayers :<: Draft Static)
          clusterByIx   ix   = cast $ index_ ix cg :: NetCluster
          genNodeLabel  node = reprStyled HeaderOnly $ uncover node

          matchCluster :: Int -> Int -> Maybe Int
          matchCluster clrIx  nix = if SubGraph.member nix (uncover $ clusterByIx clrIx) then Just clrIx else Nothing
          matchClusters :: [Int] -> Int -> [Int]
          matchClusters clrIxs nix = catMaybes $ flip matchCluster nix <$> clrIxs

          --nodeColor :: (NetLayers a :<: Draft Static) -> Attribute
          getNodeColor n = caseTest (uncover n) $ do
                                of' $ \(Lit.String   s) -> valStrNodeClr
                                of' $ \(Lit.Number _ n) -> valIntNodeClr
                                of' $ \ANY              -> nodeClr

          getLambdaNodeColor n (Function.Signature s args o) = whenSelf <|> whenArg <|> whenOut where
              whenSelf = maybe Nothing (\x -> if x ^. idx == n then Just selfClr else Nothing) s
              whenArg  = argClr <$ find ((== n) . view idx) (unlayer <$> args)
              whenOut  = if o ^. idx == n then Just outClr   else Nothing

          genSubGraph :: Int -> [Int] -> DotSubGraph String
          genSubGraph sgIdx nodeIxs = DotSG
              { isCluster     = True
              , subGraphID    = Just $ Str $ fromString $ show sgIdx
              , subGraphStmts = DotStmts { attrStmts = gStyle $ clusterByIx sgIdx # Name
                                         , subGraphs = []
                                         , nodeStmts = concat $ labeledNode (clusterByIx sgIdx # Lambda)  <$> nodeIxs
                                         , edgeStmts = []
                                         }
              }



instance IsString PortName      where fromString = PN . fromString
instance IsString Html.TextItem where fromString = Html.Str . fromString
instance IsString Html.Label    where fromString = Html.Text . (:[]) . fromString

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a


fromListWithReps :: Ord k => [(k,v)] -> Map k [v]
fromListWithReps lst = foldr update (Map.fromList initLst) lst where
    ks           = fst   <$> lst
    initLst      = (,[]) <$> ks
    update (k,v) = Map.adjust (v:) k


genInEdges (g :: NetGraph) (n :: NetLayers :<: Draft Static) = displayEdges where
    --displayEdges = tpEdge : (addColor <$> inEdges)
    displayEdges = ($ (addColor <$> inEdges) ++ redirEdge ++ replEdge) $ if t == universe then id else (<> [tpEdge])
    genLabel     = GV.XLabel . StrLabel . fromString . show
    ins          = n # Inputs
    inIxs        = view idx <$> ins
    inIdxs       = getTgtIdx <$> ins
    inEdges      = zipWith (,) inIdxs $ fmap ((:[]) . genLabel) inIxs :: [(Int, [Attribute])]
    --inEdges      = zipWith (,) inIdxs $ fmap ((:[]) . genLabel) [0..] :: [(Int, [Attribute])]
    es           = g ^. wrapped' ∘ edgeGraph
    te           = n ^. prop Type
    t            = getTgt te
    tpEdge       = (getTgtIdx te, [GV.color typedArrClr, ArrowHead dotArrow, genLabel $ te ^. idx])
    redirEdge    = maybeToList $ makeRedirEdge <$> n ^. prop TCData . redirect
    replEdge     = maybeToList $ makeReplEdge  <$> n ^. prop TCData . replacement

    makeRedirEdge e       = (getTgtIdx e, [GV.color redirectClr, Dir Back, Style [SItem Dashed []]])
    makeReplEdge c        = (view (Function.out . idx) $ fromJust $ view (prop Lambda) $ clusterByIx $ c ^. idx,
                            [GV.color redirectClr, Dir Back, Style [SItem Dashed []], LTail $ fromString $ "cluster_" ++ (show $ c ^. idx)])
    addColor (idx, attrs) = (idx, GV.color arrClr : attrs)
    getTgtIdx             = view idx ∘ getTgt
    getTgt    inp         = view source $ index (inp ^. idx) es
    clusterByIx ix        = cast $ index_ ix cg :: NetCluster
    cg                    = g ^. wrapped . Graph.subGraphs





















class Displayable m a where
    render  :: String -> a -> m ()
    display :: a -> m ()

class OpenUtility p where
    openUtility :: MonadIO m => p -> [FilePath] -> m ()

instance OpenUtility Windows where openUtility = const $ singleProcess "start"
instance OpenUtility Darwin  where openUtility = const $ singleProcess "open"
instance OpenUtility Linux   where openUtility = const $ manyProcess   "xdg-open"
instance OpenUtility GHCJS   where openUtility = const $ singleProcess "open"

singleProcess, manyProcess :: MonadIO m => String -> [FilePath] -> m ()
singleProcess p args = liftIO $ void $ createProcess $ shell $ p <> " " <> intercalate " " args
manyProcess   p = liftIO . mapM_ (\a -> createProcess $ shell $ p <> " " <> a)

open paths = openUtility platform paths


instance (MonadIO m, Ord a, PrintDot a) => Displayable m (DotGraph a) where
    render name gv = do
        let path = "/tmp/" <> name <> ".png"
        liftIO $ runGraphviz gv Png path
        return ()

    display gv = do
        let path = "/tmp/out.png"
        liftIO $ runGraphviz gv Png path
        open [path]
        return ()


-- === Utils === --

renderAndOpen lst = do
    flip mapM_ lst $ \(fname, gname, g) -> render fname $ toGraphViz gname g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap sel1 lst)
