<script lang="ts">
/// <reference types="d3" />
export const name = 'Scatterplot'
export const inputType = 'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector'

// eslint-disable-next-line no-redeclare
declare var d3: typeof import('d3')

/**
 * A d3.js Scatterplot visualization.
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
 *     "x":{"label":"x-axis label","scale":"linear"},
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

interface Data {
  axis: AxesConfiguration
  focus: Focus | undefined
  points: PointsConfiguration
  data: Point[]
}

interface Focus {
  x: number
  y: number
  zoom: number
}

interface Point {
  x: number
  y: number
  label?: string
  color?: string
  shape?: string
  size?: number
}

interface AxisConfiguration {
  label: string
  scale: Scale
}

interface PointsConfiguration {
  labels: string
}

enum Scale {
  linear = 'linear',
  logarithmic = 'logarithmic',
}

interface AxesConfiguration {
  x: AxisConfiguration
  y: AxisConfiguration
}

interface Color {
  red: number
  green: number
  blue: number
}
</script>

<script setup lang="ts">
/** Scatterplot Visualization. */
import ShowAllIcon from './icons/show_all.svg'
import FindIcon from './icons/find.svg'

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7.8.5/+esm'

import { useDocumentEvent, useDocumentEventConditional } from './events'
import { getTextWidth } from './measurement'

import VisualizationContainer from './VisualizationContainer.vue'

import { computed, onMounted, ref, watch } from 'vue'

console.log('d3', d3)

// TODO: deduplicate props.width / 40 and props.height / 20

const props = defineProps<{
  width: number | undefined
  height: number | undefined
  data: Data | string
}>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

// TODO[sb]: Consider switching to a global keyboard shortcut handler.
let shortcuts = {
  zoomIn: (e: KeyboardEvent) => (e.ctrlKey || e.metaKey) && e.key === 'z',
  showAll: (e: KeyboardEvent) => (e.ctrlKey || e.metaKey) && e.key === 'a',
}

const LABEL_FONT_STYLE = '10px DejaVuSansMonoBook'
const POINT_LABEL_PADDING_X = 7
const POINT_LABEL_PADDING_Y = 2
const ANIMATION_DURATION = 400
const VISIBLE_POINTS = 'visible'
const BUTTONS_HEIGHT = 25
const DEFAULT_LIMIT = 1024
const ACCENT_COLOR: Color = { red: 78, green: 165, blue: 253 }

const SHAPE_TO_SYMBOL: Record<string, d3.SymbolType> = {
  cross: d3.symbolCross,
  diamond: d3.symbolDiamond,
  square: d3.symbolSquare,
  star: d3.symbolStar,
  triangle: d3.symbolTriangle,
}

const SCALE_TO_D3_SCALE: Record<Scale, d3.ScaleContinuousNumeric<number, number, never>> = {
  [Scale.linear]: d3.scaleLinear(),
  [Scale.logarithmic]: d3.scaleLog(),
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
  selection: Extent | null
}

const data = computed<Data>(() => {
  let rawData: Partial<Data> | number[] =
    typeof props.data === 'string' ? JSON.parse(props.data) : props.data
  const unfilteredData = Array.isArray(rawData)
    ? rawData.map((y, index) => ({ x: index, y }))
    : rawData.data ?? []
  const data: Point[] = unfilteredData.filter(
    (point) =>
      typeof point.x === 'number' &&
      !Number.isNaN(point.x) &&
      typeof point.y === 'number' &&
      !Number.isNaN(point.y),
  )
  if (Array.isArray(rawData)) {
    rawData = {}
  }
  const axis: AxesConfiguration = rawData.axis ?? {
    x: { label: '', scale: Scale.linear },
    y: { label: '', scale: Scale.linear },
  }
  const points = rawData.points ?? { labels: 'visible' }
  const focus: Focus | undefined = rawData.focus
  return { axis, points, data, focus }
})

const containerNode = ref<HTMLElement>()
const rootNode = ref<SVGGElement>()
const pointsNode = ref<SVGGElement>()
const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()
const brushNode = ref<SVGGElement>()
const bounds = ref<[number, number, number, number] | null>(null)
const brushExtent = ref<Extent | null>(null)
const limit = ref(DEFAULT_LIMIT)

const margin = computed(() => {
  const xLabel = data.value.axis.x.label
  const yLabel = data.value.axis.y.label
  if (xLabel == null && yLabel === null) {
    return { top: 20, right: 20, bottom: 20, left: 45 }
  } else if (yLabel == null) {
    return { top: 10, right: 20, bottom: 35, left: 35 }
  } else if (xLabel == null) {
    return { top: 20, right: 10, bottom: 20, left: 55 }
  } else {
    return { top: 10, right: 10, bottom: 35, left: 55 }
  }
})
const width = computed(
  () => props.width ?? containerNode.value?.getBoundingClientRect().width ?? 100,
)
const height = computed(
  () => props.height ?? ((containerNode.value?.getBoundingClientRect().width ?? 100) * 3) / 4,
)
const xTicks = computed(() => height.value / 40)
const yTicks = computed(() => width.value / 20)
const canvasWidth = computed(() => width.value)
const canvasHeight = computed(() => Math.max(0, height.value - BUTTONS_HEIGHT))
const boxWidth = computed(() =>
  Math.max(0, canvasWidth.value - margin.value.left - margin.value.right),
)
const boxHeight = computed(() =>
  Math.max(0, canvasHeight.value - margin.value.top - margin.value.bottom),
)

function updatePreprocessor() {
  let args = []
  if (bounds.value != null) {
    args.push('[' + bounds.value.join(',') + ']')
  } else {
    args.push('Nothing')
  }
  args.push(limit.value.toString())
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
  let [xMin, xMax] = d3.extent(data.value.data, (point) => point.x)
  let [yMin, yMax] = d3.extent(data.value.data, (point) => point.y)
  xMin ??= 0
  xMax ??= 0
  yMin ??= 0
  yMax ??= 0
  const dx = xMax - xMin
  const dy = yMax - yMin
  const paddingX = 0.1 * dx
  const paddingY = 0.1 * dy
  return { xMin, xMax, yMin, yMax, paddingX, paddingY, dx, dy }
})

let scaleAndAxis = {} as ReturnType<typeof updateAxes>
let zoom = {} as ReturnType<typeof addPanAndZoom>

onMounted(() => {
  scaleAndAxis = updateAxes()
  zoom = addPanAndZoom()
  redrawPoints()
  addBrushing()
})

watch(
  () => [props.width, props.height],
  () => {
    scaleAndAxis = updateAxes()
    redrawPoints()
    addBrushing()
  },
)

/**
 * Adds panning and zooming functionality to the visualization.
 */
function addPanAndZoom() {
  if (pointsNode.value == null) {
    throw new Error('Could not find the HTML element for the scatterplot.')
  }
  const scatterplot = pointsNode.value
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
    .select(scatterplot)
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
      const rmbDivider = 100
      const zoomAmount = rmbZoomValue(event.sourceEvent) / rmbDivider
      const scale = Math.exp(zoomAmount)
      const distanceScale = getScaleForZoom(scale, startPos)
      rescale(distanceScale)
    } else if (event.sourceEvent instanceof WheelEvent) {
      if (event.sourceEvent.ctrlKey) {
        const pinchDivider = 100
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

    scaleAndAxis.xAxis.call(d3.axisBottom(transformedScale.xScale).ticks(xTicks.value))
    scaleAndAxis.yAxis.call(d3.axisLeft(transformedScale.yScale).ticks(yTicks.value))
    d3.select(scatterplot)
      .selectAll<SVGPathElement, Point>('path')
      .attr(
        'transform',
        (d) =>
          'translate(' + transformedScale.xScale(d.x) + ',' + transformedScale.yScale(d.y) + ')',
      )

    if (data.value.points.labels === VISIBLE_POINTS) {
      d3.select(scatterplot)
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
  if (brushNode.value == null) {
    throw new Error('Could not find the HTML element for the brush.')
  }
  // The brush element must be a child of zoom element - this is only way we found to have both
  // zoom and brush events working at the same time. See https://stackoverflow.com/a/59757276
  d3.select(brushNode.value).call(brush.value)
  removePointerEventsAttrsFromBrush()
}

/**
 * Removes brush, keyboard event and zoom button when end event is captured.
 */
function endBrushing() {
  if (brushNode.value == null) {
    throw new Error('Could not find the HTML element for the brush.')
  }
  brushExtent.value = null
  d3.select(brushNode.value).call(brush.value.move, null)
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
function zoomToSelected() {
  if (brushExtent.value == null) {
    return
  }
  const [[xMinRaw, yMaxRaw], [xMaxRaw, yMinRaw]] = brushExtent.value
  brushExtent.value = null
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

useDocumentEventConditional(
  'keydown',
  () => brushExtent.value != null,
  (event) => {
    if (shortcuts.zoomIn(event)) {
      zoomToSelected()
      endBrushing()
    }
  },
)

/**
 * Helper function for zooming in after the scale has been updated.
 */
function zoomingHelper(scaleAndAxis: ReturnType<typeof updateAxes>) {
  if (xAxisNode.value == null) {
    throw new Error('Could not find the HTML element for the x axis.')
  }
  if (yAxisNode.value == null) {
    throw new Error('Could not find the HTML element for the y axis.')
  }
  if (pointsNode.value == null) {
    throw new Error('Could not find the root HTML element for the scatterplot.')
  }
  d3.select(xAxisNode.value)
    .transition()
    .duration(ANIMATION_DURATION)
    .call(d3.axisBottom(scaleAndAxis.xScale).ticks(xTicks.value))
  d3.select(yAxisNode.value)
    .transition()
    .duration(ANIMATION_DURATION)
    .call(d3.axisLeft(scaleAndAxis.yScale).ticks(yTicks.value))

  d3.select(pointsNode.value)
    .selectAll<SVGPathElement, Point>('path')
    .transition()
    .duration(ANIMATION_DURATION)
    .attr(
      'transform',
      (d) => 'translate(' + scaleAndAxis.xScale(d.x) + ',' + scaleAndAxis.yScale(d.y) + ')',
    )

  if (data.value.points.labels === VISIBLE_POINTS) {
    d3.select(pointsNode.value)
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
function redrawPoints() {
  if (pointsNode.value == null) {
    throw new Error('Could not find the HTML element for the scatterplot.')
  }
  const points = pointsNode.value
  const symbol = d3.symbol()
  const sizeScaleMultiplier = 100

  const color = ACCENT_COLOR
  const fillColor = `rgba(${color.red * 255},${color.green * 255},${color.blue * 255},0.8)`

  d3.select(points)
    .selectAll('dataPoint')
    .data(data.value.data)
    .enter()
    .append('path')
    .attr(
      'd',
      symbol.type(matchShape).size((d: Point) => (d.size ?? 1.0) * sizeScaleMultiplier),
    )
    .attr(
      'transform',
      (d) => 'translate(' + scaleAndAxis.xScale(d.x) + ',' + scaleAndAxis.yScale(d.y) + ')',
    )
    .style('fill', (d) => d.color ?? fillColor)

  if (data.value.points.labels === VISIBLE_POINTS) {
    d3.select(points)
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

/**
 * Creates plot's axes.
 */
function updateAxes() {
  if (xAxisNode.value == null) {
    throw new Error('Could not find the HTML element for the x axis.')
  }
  if (yAxisNode.value == null) {
    throw new Error('Could not find the HTML element for the y axis.')
  }
  let xScale = axisD3Scale(data.value.axis.x).domain(domains.value.x).range([0, boxWidth.value])
  let xAxis = d3.select(xAxisNode.value).call(d3.axisBottom(xScale).ticks(xTicks.value))
  let yScale = axisD3Scale(data.value.axis.y).domain(domains.value.y).range([boxHeight.value, 0])
  let yAxis = d3.select(yAxisNode.value).call(d3.axisLeft(yScale).ticks(yTicks.value))
  return { xScale: xScale, yScale: yScale, xAxis: xAxis, yAxis: yAxis }
}

useDocumentEvent('keydown', (event) => {
  if (shortcuts.showAll(event)) {
    fitAll()
  }
})

function fitAll() {
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

const xLabelLeft = computed(() =>
  data.value.axis.x.label == null
    ? 0
    : margin.value.left + getTextWidth(data.value.axis.x.label, LABEL_FONT_STYLE) / 2,
)
const xLabelTop = computed(() => boxHeight.value + margin.value.top + 20)
const yLabelLeft = computed(() =>
  data.value.axis.y.label == null
    ? 0
    : -margin.value.top -
      boxHeight.value / 2 +
      getTextWidth(data.value.axis.y.label, LABEL_FONT_STYLE) / 2,
)
const yLabelTop = computed(() => -margin.value.left + 15)
</script>

<template>
  <VisualizationContainer :="<any>$attrs" :below-toolbar="true" :width="width" :height="height">
    <template #toolbar>
      <button class="image-button active">
        <img :src="ShowAllIcon" alt="Fit all" @click="fitAll" />
      </button>
      <button class="image-button" :class="{ active: brushExtent != null }">
        <img :src="FindIcon" alt="Zoom to selected" @click="zoomToSelected" />
      </button>
    </template>
    <div ref="containerNode" class="Scatterplot">
      <svg :width="canvasWidth" :height="canvasHeight">
        <g ref="rootNode" :transform="`translate(${margin.left}, ${margin.top})`">
          <defs>
            <clipPath id="clip">
              <rect :width="boxWidth" :height="boxHeight" :x="0" :y="0"></rect>
            </clipPath>
          </defs>
          <g ref="xAxisNode" class="label axis-x" :transform="`translate(0, ${boxHeight})`"></g>
          <g ref="yAxisNode" class="label axis-y"></g>
          <text
            v-if="data.axis.x.label"
            class="label label-x"
            text-anchor="end"
            :x="xLabelLeft"
            :y="xLabelTop"
            v-text="data.axis.x.label"
          ></text>
          <text
            v-if="data.axis.y.label"
            class="label label-y"
            text-anchor="end"
            :x="yLabelLeft"
            :y="yLabelTop"
            v-text="data.axis.y.label"
          ></text>
          <g ref="pointsNode" clip-path="url(#clip)"></g>
          <g class="zoom" :width="boxWidth" :height="boxHeight" fill="none">
            <g ref="brushNode" class="brush"></g>
          </g>
        </g>
      </svg>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://fonts.cdnfonts.com/css/dejavu-sans-mono');

:root {
  --color-stroke-dark: rgba(255, 255, 255, 0.7);
  --color-button-light: #333;
  --color-button-dark-hover: rgba(255, 255, 255, 0.5);
  --color-selection-fill-dark: #efefef;
}

.Scatterplot {
  user-select: none;
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
  transform: rotate(-90deg);
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

button:hover {
  background-color: var(--color-button-light);
  color: var(--color-selection-fill-dark);
}
</style>
