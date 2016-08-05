module Luna.Compilation.Pass.Interpreter.Charts where

import           Prelude.Luna
import           Text.Printf        (printf)

import           Graphics.API       as API

-- helpers

mkLayer :: Geometry -> [Transformation] -> Layer
mkLayer geo trans = Layer geo trans []

mkLayerWithLabels :: Geometry -> [Transformation] -> [Label] -> Layer
mkLayerWithLabels = Layer

withLabels :: Layer -> [Label] -> Layer
withLabels (Layer geo trans _) labels = Layer geo trans labels

toTransformation :: Point -> Transformation
toTransformation (Point x y) = translate def x y

transformToViewPoint :: Double -> Double -> Double -> Double -> Double -> Double -> Point -> Point
transformToViewPoint sx sy rx ry viewX viewY = scaleToViewPoint viewX viewY . offsetPoint rx ry . normalizePoint sx sy

scaleToViewPoint :: Double -> Double -> Point -> Point
scaleToViewPoint viewX viewY = offsetPoint offX offY . scalePoint viewX viewY . flipPointY where
    offX = 0.5 * (1.0 - viewX)
    offY = 0.5 * (1.0 - viewY)

scalePoint :: Double -> Double -> Point -> Point
scalePoint px py (Point x y) = Point (x * px) (y * py)

normalizePoint :: Double -> Double -> Point -> Point
normalizePoint sx sy (Point x y) = Point (x / sx) (y / sy)

offsetPoint :: Double -> Double -> Point -> Point
offsetPoint rx ry (Point x y) = Point (x + rx) (y + ry)

flipPointY :: Point -> Point
flipPointY (Point x y) = Point x (1.0 - y)

initialOffset :: Double -> Double -> Double
initialOffset p1 p2 = -p1 / (p2 - p1)

getOffset :: Double -> Double -> Double
getOffset w p = p / w

sampleData :: (Double -> IO Double) -> Double -> Double -> Int -> IO [Point]
sampleData f x1 x2 res = do
    let resNorm = if res < 2 then 2 else res
        nums = [0 .. (resNorm - 1)]
        range = x2 - x1
        resPrec = fromIntegral (resNorm - 1)
        xs = (\n -> x1 + range * (fromIntegral n / resPrec)) <$> nums
        ys = f <$> xs
        toPointF x = do
            y <- f x
            return $ Point x y
    mapM toPointF xs


-- drawing

figureToGeo :: Figure -> Material -> Geometry
figureToGeo figure mat = Geometry geoComp def (Just mat) where
    geoComp = GeoElem [ShapeSurface $ Shape $ Primitive figure def def]

circleToGeo :: Double -> Material -> Geometry
circleToGeo = figureToGeo . Circle

squareToGeo :: Double -> Material -> Geometry
squareToGeo = figureToGeo . Square

rectangleToGeo :: Double -> Double -> Material -> Geometry
rectangleToGeo = figureToGeo .: Rectangle

-- axes

axisWidth :: Double
axisWidth = 0.008

axisLength viewSize = viewSize + axisWidth

maxSteps :: Int
maxSteps  = 12

labelFontSize = 10.0

edgePoints :: Double -> Double -> Double -> (Double, Double)
edgePoints step p1 p2 = (p1t, p2t) where
    p1t = (* step) . fromIntegral . floor   $ p1 / step
    p2t = (* step) . fromIntegral . ceiling $ p2 / step

calculateTick :: Int -> Double -> Double
calculateTick maxTicks range = tick where
    minTick   = range / (fromIntegral (maxTicks - 2))
    magnitude = 10.0 ** (fromIntegral . floor $ logBase 10.0 minTick)
    residual  = minTick / magnitude
    tick | residual > 5.0 = 10.0 * magnitude
         | residual > 2.0 =  5.0 * magnitude
         | residual > 1.0 =  2.0 * magnitude
         | otherwise      =        magnitude

axisPoint :: Double -> Double -> Double
axisPoint p1 p2 = mp where
    step       = calculateTick maxSteps (p2 - p1)
    (p1t, p2t) = edgePoints step p1 p2
    mp         = initialOffset p1t p2t

axisH :: Material -> Double -> Double -> Double -> Layer
axisH mat viewSize y1 y2 = mkLayer geometry [toTransformation point] where
    geometry   = rectangleToGeo (axisLength viewSize) axisWidth mat
    point      = scaleToViewPoint viewSize viewSize $ Point 0.5 my
    my         = axisPoint y1 y2

axisV :: Material -> Double -> Double -> Double -> Layer
axisV mat viewSize x1 x2 = mkLayer geometry [toTransformation point] where
    geometry   = rectangleToGeo axisWidth (axisLength viewSize) mat
    point      = scaleToViewPoint viewSize viewSize $ Point mx 0.5
    mx         = axisPoint x1 x2

axes :: Material -> Double -> Double -> Double -> Double -> Double -> [Layer]
axes mat viewSize x1 x2 y1 y2 = [aH, aV] where
    aH = axisH mat viewSize y1 y2
    aV = axisV mat viewSize x1 x2

gridHStep1 :: Material -> Double -> Double -> Double -> Layer
gridHStep1 mat viewSize y1 y2 = mkLayer geometry $ toTransformation <$> points where
    geometry = rectangleToGeo (axisLength viewSize) axisWidth mat
    points   = scaleToViewPoint viewSize viewSize . Point 0.5 <$> mys
    y1i      = truncate y1
    y2i      = truncate y2
    yis      = fromIntegral <$> [y1i..y2i]
    myst     = getOffset (y2 - y1) <$> yis
    mys      = (+ initialOffset y1 y2) <$> myst

gridPoints :: Double -> Double -> [Double]
gridPoints p1 p2 = mps where
    step       = calculateTick maxSteps (p2 - p1)
    (p1t, p2t) = edgePoints step p1 p2
    actSteps   = (p2t - p1t) / step
    steps      = (\i -> i * step) <$> [0..actSteps]
    pis        = (+ p1t) <$> steps
    mpst       = getOffset (p2t - p1t) <$> pis
    mps        = (+ initialOffset p1t p2t) <$> mpst

gridH :: Material -> Double -> Double -> Double -> Layer
gridH mat viewSize y1 y2 = mkLayer geometry points where
    geometry = rectangleToGeo (axisLength viewSize) axisWidth mat
    points   = toTransformation . scaleToViewPoint viewSize viewSize . Point 0.5 <$> mys
    mys      = gridPoints y1 y2

gridV :: Material -> Double -> Double -> Double -> Layer
gridV mat viewSize x1 x2 = mkLayer geometry points where
    geometry = rectangleToGeo axisWidth (axisLength viewSize) mat
    points   = toTransformation . scaleToViewPoint viewSize viewSize . flip Point 0.5 <$> mxs
    mxs      = gridPoints x1 x2

grid :: Material -> Double -> Double -> Double -> Double -> Double -> [Layer]
grid mat viewSize x1 x2 y1 y2 = [gH, gV] where
    gH = gridH mat viewSize y1 y2
    gV = gridV mat viewSize x1 x2

labelOff viewSize = 0.5 * (1.0 - viewSize)

labelOffX = -0.04
labelOffY = -0.025

labelAdjustX = 0.0
labelAdjustY = 0.035

showLabel :: Int -> Double -> String
showLabel decim = printf $ "%0." <> show decim <> "f"

gridLabeledH :: Material -> Int -> Double -> Double -> Double -> Layer
gridLabeledH mat decim viewSize y1 y2 = mkLayerWithLabels geometry points labels where
    geometry = rectangleToGeo (axisLength viewSize) axisWidth mat
    points   = toTransformation . scaleToViewPoint viewSize viewSize . Point 0.5 <$> mys
    labels   = mkLabel <$> mys
    mys      = gridPoints y1 y2
    stepY      = calculateTick maxSteps (y2 - y1)
    (y1t, y2t) = edgePoints stepY y1 y2
    mkLabel y = Label pos (labelFontSize * viewSize) API.Right $ showLabel decim ay where
        ay    = y1t + y * (y2t - y1t)
        pos   = scaleToViewPoint viewSize viewSize $ Point labelOffX (y + labelAdjustY)

gridLabeledV :: Material -> Int -> Double -> Double -> Double -> Layer
gridLabeledV mat decim viewSize x1 x2 = mkLayerWithLabels geometry points labels where
    geometry = rectangleToGeo axisWidth (axisLength viewSize) mat
    points   = toTransformation . scaleToViewPoint viewSize viewSize . flip Point 0.5 <$> mxs
    labels   = mkLabel <$> mxs
    mxs      = gridPoints x1 x2
    stepX      = calculateTick maxSteps (x2 - x1)
    (x1t, x2t) = edgePoints stepX x1 x2
    mkLabel x = Label pos (labelFontSize * viewSize) API.Center $ showLabel decim ax where
        ax    = x1t + x * (x2t - x1t)
        pos   = scaleToViewPoint viewSize viewSize $ Point (x + labelAdjustX) labelOffY

gridLabeled :: Material -> Int -> Double -> Double -> Double -> Double -> Double -> [Layer]
gridLabeled mat decim viewSize x1 x2 y1 y2 = [gH, gV] where
    gH = gridLabeledH mat decim viewSize y1 y2
    gV = gridLabeledV mat decim viewSize x1 x2

chartShift = Point 0.03 (-0.03)

shiftPoint :: Double -> Double -> Point -> Point
shiftPoint viewX viewY (Point x y) = Point (viewX * x) (viewY * y)

shiftGraphics :: Point -> Graphics -> Graphics
shiftGraphics point (Graphics layers) = Graphics layers' where
    layers'  = shiftLayer point <$> layers

shiftLayer :: Point -> Layer -> Layer
shiftLayer point (Layer geo trans labels) = Layer geo trans' labels' where
    trans'  = shiftTrans point <$> trans
    labels' = shiftLabel point <$> labels
    shiftTrans (Point x y) transformation = translate transformation x y
    shiftLabel (Point x y) (Label (Point lx ly) fontSize alignment text) = Label (Point (lx + x) (ly + y)) fontSize alignment text

-- auto charts

autoScatterChartInt :: Material -> Material -> Figure -> Double -> [Int] -> Graphics
autoScatterChartInt gridMat mat figure viewSize ints = shiftGraphics shift chart where
    chart = autoScatterChartDoubleImpl gridMat mat figure 0 viewSize $ fromIntegral <$> ints
    shift = shiftPoint viewSize viewSize chartShift

autoScatterChartDouble :: Material -> Material -> Figure -> Double -> [Double] -> Graphics
autoScatterChartDouble gridMat mat figure viewSize doubles = shiftGraphics shift chart where
    chart = autoScatterChartDoubleImpl gridMat mat figure 1 viewSize doubles
    shift = shiftPoint viewSize viewSize chartShift

autoScatterChartIntTuple :: Material -> Material -> Figure -> Double -> [(Int, Int)] -> Graphics
autoScatterChartIntTuple gridMat mat figure viewSize intTuples = shiftGraphics shift chart where
    chart = autoScatterChartDoubleTupleImpl gridMat mat figure viewSize $ toDoubleTuple <$> intTuples
    shift = shiftPoint viewSize viewSize chartShift
    toDoubleTuple :: (Int, Int) -> (Double, Double)
    toDoubleTuple (int1, int2) = (fromIntegral int1, fromIntegral int2)

autoScatterChartDoubleTuple :: Material -> Material -> Figure -> Double -> [(Double, Double)] -> Graphics
autoScatterChartDoubleTuple gridMat mat figure viewSize doubleTuples = shiftGraphics shift chart where
    chart = autoScatterChartDoubleTupleImpl gridMat mat figure viewSize doubleTuples
    shift = shiftPoint viewSize viewSize chartShift

autoScatterChartDoubleImpl :: Material -> Material -> Figure -> Int -> Double -> [Double] -> Graphics
autoScatterChartDoubleImpl gridMat mat figure decim viewSize doublesY = autoScatterChartDoubleTupleImpl gridMat mat figure viewSize $ zip [0.0..] doublesY

autoScatterChartDoubleTupleImpl :: Material -> Material -> Figure -> Double -> [(Double, Double)] -> Graphics
autoScatterChartDoubleTupleImpl gridMat mat figure viewSize []        = Graphics []
autoScatterChartDoubleTupleImpl gridMat mat figure viewSize doublesXY = Graphics $ gridLayer <> [chartLayer] where
    chartLayer = scatterChart mat figure viewSize x1 x2 y1 y2 points
    gridLayer  = grid gridMat viewSize x1 x2 y1 y2
    doublesX   = fst <$> doublesXY
    doublesY   = snd <$> doublesXY
    x1 = min 0.0 $ minimum doublesX
    x2 = max 0.0 $ maximum doublesX
    y1 = min 0.0 $ minimum doublesY
    y2 = max 0.0 $ maximum doublesY
    points = (\(i, v) -> Point i v) <$> doublesXY

-- charts

scatterChart :: Material -> Figure -> Double -> Double -> Double -> Double -> Double -> [Point] -> Layer
scatterChart mat figure viewSize x1 x2 y1 y2 points = scatterChartImpl geometry viewPoints where
    geometry   = figureToGeo figure mat
    viewPoints = transformToViewPoint (x2t - x1t) (y2t - y1t) rx ry viewSize viewSize <$> points
    rx         = initialOffset x1t x2t
    ry         = initialOffset y1t y2t
    stepX      = calculateTick maxSteps (x2 - x1)
    stepY      = calculateTick maxSteps (y2 - y1)
    (x1t, x2t) = edgePoints stepX x1 x2
    (y1t, y2t) = edgePoints stepY y1 y2

barChart :: Material -> Double -> Double -> Double -> Double -> Double -> [Point] -> Layer
barChart mat viewSize x1 x2 y1 y2 points = barChartImpl mat viewPoints where
    viewPoints = transformToViewPoint (x2 - x1) (y2 - y1) rx ry viewSize viewSize <$> points
    rx         = initialOffset x1 x2
    ry         = initialOffset y1 y2

barChartLayers :: Material -> Double -> Double -> Double -> Double -> Double -> [Point] -> Graphics
barChartLayers mat viewSize x1 x2 y1 y2 points = Graphics layers where
    layers     = toLayer <$> viewPoints
    viewPoints = transformToViewPoint (x2 - x1) (y2 - y1) rx ry viewSize viewSize <$> points
    rx         = initialOffset x1 x2
    ry         = initialOffset y1 y2
    w          = 0.5 / (fromIntegral $ length points)
    toLayer (Point dx dy) = mkLayer geometry [toTransformation point] where
        point     = Point dx (dy * 0.5)
        geometry  = Geometry geoComp def (Just mat)
        geoComp   = convert figure :: GeoComponent
        figure    = Rectangle w h
        h         = abs dy

-- charts helpers

scatterChartImpl :: Geometry -> [Point] -> Layer
scatterChartImpl geometry points = mkLayer geometry $ toTransformation <$> points

barChartImpl = barChartGeometriesImpl

-- TODO: barChartShapes :: Material -> [Transformation] -> Layer
-- rewrite to new2 API with composable list of shapes with transformations

-- TODO: check why this hangs
-- TODO: check why composing geometries does not work with translations
barChartGeometriesImpl :: Material -> [Point] -> Layer
barChartGeometriesImpl mat points = layer where
    pointsX          = transformX <$> points
    pointsY          = transformY <$> points
    geoComponent     = GeoElem  $ toSurface  <$> pointsY
    geoComponentMain = GeoGroup $ toGeometry <$> pointsX
    geometry         = Geometry geoComponentMain def $ Just mat
    layer            = mkLayer geometry [def]
    toGeometry :: Point -> Geometry
    toGeometry point = Geometry geoComponent (toTransformation point) $ Just mat
    toSurface :: Point -> Surface
    toSurface  (Point _  dy) = ShapeSurface $ Shape $ Primitive (Rectangle 0.02 (abs dy)) def def
    transformX (Point dx dy) = Point dx  0.0
    transformY (Point dx dy) = Point 0.0 dy
