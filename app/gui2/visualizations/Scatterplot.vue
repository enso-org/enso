<script setup lang="ts">
/** ScatterPlot Visualization. */
import { useDocumentEvent, useDocumentEventConditional } from '@/util/events'
import { getTextWidth } from '@/util/measurement'
import { isValidNumber } from '@/util/visualizationHelpers'

import Visualization from '@viz/Visualization.vue'
// FIXME: either move this to @viz, or change to a different impl
import { registerVisualization } from '@/util/visualizations'

import * as d3 from 'd3'
import { computed, onMounted, ref, watchEffect } from 'vue'

registerVisualization(
  'Scatterplot',
  'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector',
)

// TODO refactor this to avoid loading scripts on startup. See issue #985.
/**
 * A d3.js ScatterPlot visualization.
 *
 * To zoom use scroll wheel.
 * To select click and swipe with LMB.
 * To deselect click outside of selection with LMB.
 * To pan click and swipe with RMB.
 * To zoom out click "Fit all" or use key combination "ctrl/cmd+a".
 * To zoom into selection click appropriate button or use key combination "ctrl/cmd+z".
 *
 * Data format (JSON):
 * {
 *  "axis":{
 *     "x":{"label":"x-axis label""scale":"linear"},
 *     "y":{"label":"y-axis label","scale":"logarithmic"},
 *  },
 *  "focus":{"x":1.7,"y":2.1,"zoom":3.0},
 *  "points":{"labels":"visible" | "invisible"},
 *  "data":[
 *     {"x":0.1,"y":0.7,"label":"foo","color":"FF0000","shape":"circle","size":0.2},
 *     ...
 *     {"x":0.4,"y":0.2,"label":"baz","color":"0000FF","shape":"square","size":0.3}
 *  ]
 * }
 */

// TODO: deduplicate props.width / 40 and props.height / 20

const props = defineProps<{
  isCircularMenuVisible: boolean
  data: Data
  defaultPointColor: Color // Was previously `theme.get('accent')`
  // FIXME: should height and width be slot props
  height: number
  width: number
  xLabel?: string
  yLabel?: string
}>()
const emit = defineEmits<{
  hide: []
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

interface Color {
  red: number
  green: number
  blue: number
}

// TODO[sb]: Consider switching to a global keyboard shortcut handler.
let shortcuts = {
  zoomIn: (e: KeyboardEvent) => (e.ctrlKey || e.metaKey) && e.key === 'z',
  showAll: (e: KeyboardEvent) => (e.ctrlKey || e.metaKey) && e.key === 'a',
}

enum Scale {
  linear = 'linear',
  logarithmic = 'logarithmic',
}

const LABEL_FONT_STYLE = '10px DejaVuSansMonoBook'
const X_AXIS_LABEL_WIDTH = 30
const POINT_LABEL_PADDING_X = 7
const POINT_LABEL_PADDING_Y = 2
const ANIMATION_DURATION = 1000
const VISIBLE_POINTS = 'visible'
const BUTTONS_HEIGHT = 25
const DEFAULT_LIMIT = 1024

const SHAPE_TO_SYMBOL: Record<string, d3.SymbolType> = {
  cross: d3.symbolCross,
  diamond: d3.symbolDiamond,
  square: d3.symbolSquare,
  star: d3.symbolStar,
  triangle: d3.symbolTriangle,
}

const SCALE_TO_D3_SCALE: Record<Scale, d3.AxisScale<number>> = {
  [Scale.linear]: d3.scaleLinear(),
  [Scale.logarithmic]: d3.scaleLog(),
}

interface Focus {
  x: number
  y: number
  zoom: number
}

interface Point {
  x: number
  y: number
  color?: string
  label?: string
}

interface AxisConfiguration {
  scale: Scale
}

interface AxesConfiguration {
  x: AxisConfiguration
  y: AxisConfiguration
}

interface Data {
  axis: AxesConfiguration
  focus: Focus | undefined
  data: Point[]
  points: { labels: string }
}

interface D3Event {
  sourceEvent: Event | null
}

interface D3ZoomEvent extends D3Event {
  transform: d3.ZoomTransform
}

interface D3ZoomStartEvent extends D3Event {
  sourceEvent: WheelEvent
}

type Extent = [topLeft: [left: number, top: number], bottomRight: [right: number, bottom: number]]

interface D3BrushEvent extends D3Event {
  selection: Extent
}

const data = computed<Data>(() => {
  let rawData: Partial<Data> | number[] =
    typeof props.data === 'string' ? JSON.parse(props.data) : props.data
  const unfilteredData = Array.isArray(rawData)
    ? rawData.map((y, index) => ({ x: index, y }))
    : rawData.data ?? []
  const data = unfilteredData.filter((point) => isValidNumber(point.x) && isValidNumber(point.y))
  if (Array.isArray(rawData)) {
    rawData = {}
  }
  const axis = rawData.axis ?? {
    x: { scale: Scale.linear },
    y: { scale: Scale.linear },
  }
  const points = rawData.points ?? { labels: 'visible' }
  const focus = rawData.focus
  return { axis, points, data, focus }
})

// FIXME: remove `dom`
const dom = ref<HTMLElement>()
const containerNode = ref<HTMLElement>()
const svgNode = ref<SVGGElement>()
const scatterplotNode = ref<SVGGElement>()
const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()
const brushNode = ref<SVGGElement>()
const bounds = ref<[number, number, number, number] | null>(null)
const brushExtent = ref<Extent>([
  [0, 0],
  [0, 0],
])
const limit = ref(DEFAULT_LIMIT)
const isZoomToSelectedVisible = ref(false)

const margin = computed(() => {
  if (props.xLabel == null && props.yLabel === null) {
    return { top: 20, right: 20, bottom: 20, left: 45 }
  } else if (props.yLabel == null) {
    return { top: 10, right: 20, bottom: 35, left: 35 }
  } else if (props.xLabel == null) {
    return { top: 20, right: 10, bottom: 20, left: 55 }
  } else {
    return { top: 10, right: 10, bottom: 35, left: 55 }
  }
})
const canvasWidth = computed(() => props.width)
const canvasHeight = computed(() => props.height - BUTTONS_HEIGHT)
// FIXME: why is this subtracting both left and right?
const boxWidth = computed(() => canvasWidth.value - margin.value.left - margin.value.right)
const boxHeight = computed(() => canvasHeight.value - margin.value.top - margin.value.bottom)

function updatePreprocessor() {
  let args = []
  if (bounds.value != null) {
    args.push('[' + bounds.value.join(',') + ']')
  } else {
    args.push('Nothing')
  }
  args.push(limit.toString())
  emit(
    'update:preprocessor',
    'Standard.Visualization.Scatter_Plot',
    'process_to_json_text',
    ...args,
  )
}

onMounted(() => {
  updatePreprocessor()
})

/**
 * Helper function calculating extreme values and paddings to make sure data will fit nicely.
 *
 * It traverses through data getting minimal and maximal values, and calculates padding based on
 * span calculated from above values, multiplied by 10% so that the plot is a little bit smaller
 * than the container.
 */
const extremesAndDeltas = computed(() => {
  let xMin = -Infinity
  let xMax = Infinity
  let yMin = -Infinity
  let yMax = Infinity
  {
    const xs = data.value.data.map((point) => point.x)
    xMin = Math.min(...xs)
    xMax = Math.max(...xs)
  }
  {
    const ys = data.value.data.map((point) => point.y)
    yMin = Math.min(...ys)
    yMax = Math.max(...ys)
  }
  const dx = xMax - xMin
  const dy = yMax - yMin
  const paddingX = 0.1 * dx
  const paddingY = 0.1 * dy
  return { xMin, xMax, yMin, yMax, paddingX, paddingY, dx, dy }
})

// FIXME: scaleAndAxis
const scaleAndAxis = updateAxes()
function drawScatterplot() {
  updateScatter()
  updateAxes()
  addBrushing()
}

/**
 * Adds panning and zooming functionality to the visualization.
 */
function addPanAndZoom() {
  const minScale = 0.5
  const maxScale = 20
  const rightButton = 2
  const midButton = 1
  const midButtonClicked = 4
  const scrollWheel = 0
  const extent: [min: number, max: number] = [minScale, maxScale]
  let startPos: Point
  const zoom = d3
    .zoom<SVGGElement, unknown>()
    .filter((event: Event) => {
      if (
        event instanceof MouseEvent &&
        event.type === 'mousedown' &&
        (event.button === rightButton || event.button === midButton)
      ) {
        return true
      } else if (
        event instanceof WheelEvent &&
        event.type === 'wheel' &&
        event.button === scrollWheel
      ) {
        return true
      } else {
        return false
      }
    })
    .wheelDelta((event) => {
      const minDelta = 0.002
      const medDelta = 0.05
      const maxDelta = 1
      const wheelSpeedMultiplier =
        event.deltaMode === 1 ? medDelta : event.deltaMode ? maxDelta : minDelta
      return -event.deltaY * wheelSpeedMultiplier
    })
    .scaleExtent(extent)
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('zoom', zoomed)
    .on('start', startZoom)

  const zoomElem = d3
    .select(scatterplotNode.value!)
    .append('g')
    .attr('class', 'zoom')
    .attr('width', boxWidth.value)
    .attr('height', boxHeight.value)
    .style('fill', 'none')
    .call(zoom)

  let transformedScale = { ...scaleAndAxis }
  let tempRmbScale = { ...scaleAndAxis }

  /**
   * Helper function called on pan/scroll.
   */
  function zoomed(event: D3ZoomEvent) {
    function rescale(distanceScale: d3.ZoomTransform) {
      transformedScale.xScale = distanceScale.rescaleX(transformedScale.xScale)
      transformedScale.yScale = distanceScale.rescaleY(transformedScale.yScale)
    }

    function getScaleForZoom(scale: number, focus: Point) {
      return d3.zoomIdentity
        .translate(focus.x - margin.value.left, focus.y - margin.value.top)
        .scale(scale)
        .translate(-focus.x + margin.value.left, -focus.y + margin.value.top)
    }

    if (event.sourceEvent instanceof MouseEvent && event.sourceEvent.buttons === rightButton) {
      transformedScale.xScale = tempRmbScale.xScale
      transformedScale.yScale = tempRmbScale.yScale
      const rmbDivider = 100.0
      const zoomAmount = rmbZoomValue(event.sourceEvent) / rmbDivider
      const scale = Math.exp(zoomAmount)
      const distanceScale = getScaleForZoom(scale, startPos)
      rescale(distanceScale)
    } else if (event.sourceEvent instanceof WheelEvent) {
      if (event.sourceEvent.ctrlKey) {
        const pinchDivider = 100.0
        const zoomAmount = -event.sourceEvent.deltaY / pinchDivider
        const scale = Math.exp(zoomAmount)
        const distanceScale = getScaleForZoom(scale, startPos)
        rescale(distanceScale)
      } else {
        const distanceScale = d3.zoomIdentity.translate(
          -event.sourceEvent.deltaX,
          -event.sourceEvent.deltaY,
        )
        rescale(distanceScale)
      }
    } else if (
      event.sourceEvent instanceof MouseEvent &&
      event.sourceEvent.buttons === midButtonClicked
    ) {
      const movementFactor = 2
      const distanceScale = d3.zoomIdentity.translate(
        event.sourceEvent.movementX / movementFactor,
        event.sourceEvent.movementY / movementFactor,
      )
      rescale(distanceScale)
    } else {
      rescale(event.transform)
    }

    scaleAndAxis.xAxis.call(d3.axisBottom(transformedScale.xScale).ticks(props.width / 40))
    scaleAndAxis.yAxis.call(d3.axisLeft(transformedScale.yScale).ticks(props.height / 20))
    d3.select(scatterplotNode.value!)
      .selectAll<SVGPathElement, Point>('path')
      .attr(
        'transform',
        (d) =>
          'translate(' + transformedScale.xScale(d.x) + ',' + transformedScale.yScale(d.y) + ')',
      )

    if (data.value.points.labels === VISIBLE_POINTS) {
      d3.select(scatterplotNode.value!)
        .selectAll<SVGTextElement, Point>('text')
        .attr('x', (d) => transformedScale.xScale(d.x) + POINT_LABEL_PADDING_X)
        .attr('y', (d) => transformedScale.yScale(d.y) + POINT_LABEL_PADDING_Y)
    }
  }

  /**
   * Return the position of this event in local canvas coordinates.
   */
  function getPos(event: MouseEvent | WheelEvent | null) {
    if (event != null) {
      return { x: event.offsetX, y: event.offsetY }
    }
    return { x: 0, y: 0 }
  }

  /**
   * Return the zoom value computed from the initial right-mouse-button event to the current
   * right-mouse event.
   */
  function rmbZoomValue(event: MouseEvent) {
    const end = getPos(event)
    const dX = end.x - startPos.x
    const dY = end.y - startPos.y
    return dX - dY
  }

  /**
   * Helper function called when starting to pan/scroll.
   */
  function startZoom(event: D3ZoomStartEvent) {
    startPos = getPos(event.sourceEvent)
    tempRmbScale = { ...transformedScale }
  }

  return { zoomElem, zoom, transformedScale }
}

/** Removing `pointer-events` handling from brush element, as we want it to be inherited. D3 inserts
 * `pointer-events: all` in the brush element and some of its children on brush creation and after brushing ends.
 * There is no documentation on that topic as far as we are aware, so this was observed and tested manually. */
function removePointerEventsAttrsFromBrush() {
  if (brushNode.value != null) {
    brushNode.value.removeAttribute('pointer-events')
    for (const child of brushNode.value.children) {
      child.removeAttribute('pointer-events')
    }
  }
}

const brush = computed(() =>
  d3
    .brush()
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('start brush', (event: D3BrushEvent) => {
      isZoomToSelectedVisible.value = true
      brushExtent.value = event.selection
    }),
)

/**
 * Adds brushing functionality to the plot.
 *
 * Brush is a tool which enables user to select points, and zoom into selection via
 * keyboard shortcut or button event.
 */
function addBrushing() {
  // The brush element must be a child of zoom element - this is only way we found to have both
  // zoom and brush events working at the same time. See https://stackoverflow.com/a/59757276
  d3.select(brushNode.value!).call(brush.value)
  removePointerEventsAttrsFromBrush()
}

/**
 * Removes brush, keyboard event and zoom button when end event is captured.
 */
function endBrushing() {
  isZoomToSelectedVisible.value = false
  d3.select(brushNode.value!).call(brush.value.move, null)
  removePointerEventsAttrsFromBrush()
}

useDocumentEvent('click', endBrushing)
useDocumentEvent('auxclick', endBrushing)
useDocumentEvent('contextmenu', endBrushing)
useDocumentEvent('scroll', endBrushing)

/**
 * Zooms into the selected fragment of the plot.
 *
 * Based on https://www.d3-graph-gallery.com/graph/interactivity_brush.html
 * Section "Brushing for zooming".
 */
function zoomIn() {
  isZoomToSelectedVisible.value = false
  const [[xMinRaw, yMaxRaw], [xMaxRaw, yMinRaw]] = brushExtent.value
  let xMin = zoom.transformedScale.xScale.invert(xMinRaw)
  let xMax = zoom.transformedScale.xScale.invert(xMaxRaw)
  let yMin = zoom.transformedScale.yScale.invert(yMinRaw)
  let yMax = zoom.transformedScale.yScale.invert(yMaxRaw)

  bounds.value = [xMin, yMin, xMax, yMax]
  updatePreprocessor()

  zoom.transformedScale.xScale.domain([xMin, xMax])
  zoom.transformedScale.yScale.domain([yMin, yMax])

  zoomingHelper(zoom.transformedScale)
}

useDocumentEventConditional('keydown', isZoomToSelectedVisible, (event) => {
  if (shortcuts.zoomIn(event)) {
    zoomIn()
    endBrushing()
  }
})

/**
 * Helper function for zooming in after the scale has been updated.
 */
function zoomingHelper(scaleAndAxis: ReturnType<typeof updateAxes>) {
  d3.select(xAxisNode.value!)
    .transition()
    .duration(ANIMATION_DURATION)
    .call(d3.axisBottom(scaleAndAxis.xScale).ticks(props.width / 40))
  d3.select(yAxisNode.value!)
    .transition()
    .duration(ANIMATION_DURATION)
    .call(d3.axisLeft(scaleAndAxis.yScale).ticks(props.height / 20))

  d3.select(scatterplotNode.value!)
    .selectAll<SVGPathElement, Point>('path')
    .transition()
    .duration(ANIMATION_DURATION)
    .attr(
      'transform',
      (d) => 'translate(' + scaleAndAxis.xScale(d.x) + ',' + scaleAndAxis.yScale(d.y) + ')',
    )

  if (data.value.points.labels === VISIBLE_POINTS) {
    d3.select(scatterplotNode.value!)
      .selectAll<SVGTextElement, Point>('text')
      .transition()
      .duration(ANIMATION_DURATION)
      .attr('x', (d) => scaleAndAxis.xScale(d.x) + POINT_LABEL_PADDING_X)
      .attr('y', (d) => scaleAndAxis.yScale(d.y) + POINT_LABEL_PADDING_Y)
  }
}

/**
 * Create a plot object and populate it with the given data.
 */
function updateScatter() {
  const symbol = d3.symbol()
  const sizeScaleMultiplier = 100

  const color = props.defaultPointColor
  const fillColor = `rgba(${color.red * 255},${color.green * 255},${color.blue * 255},0.8)`

  d3.select(svgNode.value!)
    .selectAll('dataPoint')
    .data(data.value.data)
    .enter()
    .append('path')
    .attr(
      'd',
      symbol.type(matchShape).size((d) => (d.size ?? 1.0) * sizeScaleMultiplier),
    )
    .attr(
      'transform',
      (d) => 'translate(' + scaleAndAxis.xScale(d.x) + ',' + scaleAndAxis.yScale(d.y) + ')',
    )
    .style('fill', (d) => d.color ?? fillColor)

  if (data.value.points.labels === VISIBLE_POINTS) {
    d3.select(svgNode.value!)
      .selectAll('dataPoint')
      .data(data.value.data)
      .enter()
      .append('text')
      .text((d) => d.label ?? '')
      .attr('x', (d) => scaleAndAxis.xScale(d.x) + POINT_LABEL_PADDING_X)
      .attr('y', (d) => scaleAndAxis.yScale(d.y) + POINT_LABEL_PADDING_Y)
      .attr('class', 'label')
      .attr('fill', 'black')
  }
}

/**
 * Helper function to match d3 shape from string.
 */
function matchShape(d: { shape: string }) {
  return SHAPE_TO_SYMBOL[d.shape] ?? d3.symbolCircle
}

/**
 * Construct either linear or logarithmic D3 scale.
 *
 * The scale kind is selected depending on update contents.
 *
 * @param axis Axis information as received in the visualization update.
 * @returns D3 scale.
 */
function axisD3Scale(axis: AxisConfiguration | undefined) {
  return axis != null ? SCALE_TO_D3_SCALE[axis.scale] : d3.scaleLinear()
}

/**
 * Helper function calculating domains for x and y axes.
 *
 * Example:
 * Lets say we have a bunch of points. Those points will have some minimum and maximum value,
 * from which we can calculate the span of points on X and Y axis, hence domain, with added
 * padding to make sure points will fit nicely on the chart.
 */
const domains = computed(() => {
  let domainX = [
    extremesAndDeltas.value.xMin - extremesAndDeltas.value.paddingX,
    extremesAndDeltas.value.xMax + extremesAndDeltas.value.paddingX,
  ]
  let domainY = [
    extremesAndDeltas.value.yMin - extremesAndDeltas.value.paddingY,
    extremesAndDeltas.value.yMax + extremesAndDeltas.value.paddingY,
  ]

  const focus = data.value.focus
  if (focus != null) {
    if (focus.x != null && focus.y != null && focus.zoom != null) {
      let newPaddingX = extremesAndDeltas.value.dx * (1 / (2 * focus.zoom))
      let newPaddingY = extremesAndDeltas.value.dy * (1 / (2 * focus.zoom))
      domainX = [focus.x - newPaddingX, focus.x + newPaddingX]
      domainY = [focus.y - newPaddingY, focus.y + newPaddingY]
    }
  }
  return { x: domainX, y: domainY }
})

// FIXME: vue-ify
/**
 * Creates plot's axes.
 */
function updateAxes() {
  if (xAxisNode.value == null) {
    throw new Error('Could not find HTML element for the x axis.')
  } else if (yAxisNode.value == null) {
    throw new Error('Could not find HTML element for the y axis.')
  }

  let xScale = axisD3Scale(data.value.axis?.x) as d3.ScaleLinear<number, number, never>
  xScale.domain(domains.value.x)
  xScale.range([0, boxWidth.value])
  let xAxis = d3.select(xAxisNode.value!).call(d3.axisBottom(xScale).ticks(props.width / 40))

  let yScale = axisD3Scale(data.value.axis?.y) as d3.ScaleLinear<number, number, never>
  yScale.domain(domains.value.y)
  yScale.range([boxHeight.value, 0])
  let yAxis = d3.select(yAxisNode.value!).call(d3.axisLeft(yScale).ticks(props.height / 20))
  return { xScale: xScale, yScale: yScale, xAxis: xAxis, yAxis: yAxis }
}

useDocumentEvent('keydown', (event) => {
  if (shortcuts.showAll(event)) {
    unzoom()
  }
})

// FIXME: vue-ify
const zoom = addPanAndZoom()

function unzoom() {
  zoom.zoomElem.transition().duration(0).call(zoom.zoom.transform, d3.zoomIdentity)

  let domainX = [
    extremesAndDeltas.value.xMin - extremesAndDeltas.value.paddingX,
    extremesAndDeltas.value.xMax + extremesAndDeltas.value.paddingX,
  ]
  let domainY = [
    extremesAndDeltas.value.yMin - extremesAndDeltas.value.paddingY,
    extremesAndDeltas.value.yMax + extremesAndDeltas.value.paddingY,
  ]

  zoom.transformedScale.xScale.domain(domainX)
  zoom.transformedScale.yScale.domain(domainY)

  zoomingHelper(zoom.transformedScale)

  bounds.value = null
  limit.value = DEFAULT_LIMIT
  updatePreprocessor()
}

watchEffect(() => {
  dom.value?.setAttribute('width', String(props.width))
  drawScatterplot()
})

watchEffect(() => {
  dom.value?.setAttribute('height', String(props.height))
  drawScatterplot()
})

const xLabelLeft = computed(() =>
  props.xLabel == null ? 0 : margin.value.left + getTextWidth(props.xLabel, LABEL_FONT_STYLE) / 2,
)
const xLabelTop = computed(() => boxHeight.value + margin.value.top + 20)
const yLabelLeft = computed(() =>
  props.yLabel == null
    ? 0
    : -margin.value.top - boxHeight.value / 2 + getTextWidth(props.yLabel, LABEL_FONT_STYLE) / 2,
)
const yLabelTop = computed(() => -margin.value.left + 15)
</script>

<template>
  <Visualization :is-circular-menu-visible="isCircularMenuVisible">
    <div class="Scatterplot" ref="containerNode">
      <svg :width="canvasWidth" :height="canvasHeight">
        <g ref="svgNode" :transform="`translate(${margin.left}, ${margin.top})`">
          <defs>
            <clipPath id="clip">
              <rect :width="boxWidth" :height="boxHeight" :x="0" :y="0"></rect>
            </clipPath>
          </defs>
          <g ref="xAxisNode" class="label axis-x" :transform="`translate(0, ${boxHeight})`"></g>
          <g ref="yAxisNode" class="label axis-y" :transform="`translate(0, ${boxHeight})`"></g>
          <text
            v-if="xLabel"
            class="label label-x"
            text-anchor="end"
            :x="xLabelLeft"
            :y="xLabelTop"
            v-text="xLabel"
          ></text>
          <text
            v-if="yLabel"
            class="label label-y"
            text-anchor="end"
            :x="yLabelLeft"
            :y="yLabelTop"
            v-text="yLabel"
          ></text>
          <g ref="scatterplotNode" clip-path="url(#clip)"></g>
          <!-- FIXME: pan -->
          <g class="zoom" :width="boxWidth" :height="boxHeight" fill="none">
            <g ref="brushNode" class="brush"></g>
          </g>
        </g>
      </svg>
      <button class="fit-all-button" @click="unzoom">Fit all</button>
      <button v-if="isZoomToSelectedVisible" class="zoom-to-selected-button" @click="zoomIn">
        Zoom to selected
      </button>
    </div>
  </Visualization>
</template>

<style scoped>
@import url('https://fonts.cdnfonts.com/css/dejavu-sans-mono');

:root {
  --color-stroke-dark: rgba(255, 255, 255, 0.7);
  --color-button-light: #333;
  --color-button-dark-hover: rgba(255, 255, 255, 0.5);
  --color-selection-fill-dark: #efefef;
}

.fit-all-button {
  width: 80px;
  height: 20px;
}

.zoom-to-selected-button {
  width: 120px;
  height: 20px;
}

.label {
  font-family: DejaVuSansMonoBook;
  font-size: 10px;
}

.label-y {
  transform: rotate(-90);
}

.Scatterplot .selection {
  rx: 4px;
  stroke: transparent;
}

.Scatterplot button {
  margin-left: 5px;
  margin-bottom: 5px;
  display: inline-block;
  padding: 2px 10px;
  outline: none;
  background-color: transparent;
  border: 1px solid var(--color-button-light);
  color: var(--color-button-light);
  border-radius: 14px;
  font-size: 10px;
  font-family: DejaVuSansMonoBook;
  vertical-align: top;
  transition: all 0.3s ease;
}

.vis-scatterplot button:hover {
  background-color: var(--color-button-light);
  color: var(--color-selection-fill-dark);
}

/* FIXME[sb]: Dark theme is currently not supported. */
.dark-theme .vis-scatterplot button {
  border: 0;
  background-color: var(--color-stroke-dark);
}

.dark-theme .vis-scatterplot button:hover {
  background-color: var(--color-button-dark-hover);
}

.dark-theme .vis-scatterplot .selection {
  fill: var(--color-selection-fill-dark);
}

.dark-theme .vis-scatterplot line {
  stroke: var(--color-stroke-dark);
}

.dark-theme .vis-scatterplot .domain {
  stroke: var(--color-stroke-dark);
}

.dark-theme .vis-scatterplot text {
  fill: var(--color-stroke-dark);
}
</style>
