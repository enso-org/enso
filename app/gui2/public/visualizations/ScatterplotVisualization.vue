<script lang="ts">
export const name = 'Scatterplot'
export const inputType = 'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector'

// eslint-disable-next-line no-redeclare
declare const d3: typeof import('d3')

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

interface PointsConfiguration {
  labels: string
}

enum ScaleType {
  Linear = 'linear',
  Logarithmic = 'logarithmic',
}

interface AxisConfiguration {
  label: string
  scale: ScaleType
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
import { computed, onMounted, ref, watch, watchEffect, watchPostEffect } from 'vue'

import FindIcon from './icons/find.svg'
import ShowAllIcon from './icons/show_all.svg'

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7.8.5/+esm'

import type {
  BrushSelection,
  D3BrushEvent,
  D3ZoomEvent,
  ScaleContinuousNumeric,
  SymbolType,
} from 'd3'

import { useEvent, useEventConditional } from './events.ts'
import { getTextWidth } from './measurement.ts'

import VisualizationContainer from 'builtins/VisualizationContainer.vue'
import { useVisualizationConfig } from 'builtins/useVisualizationConfig.ts'

import type { Symbol } from 'd3'

const props = defineProps<{ data: Partial<Data> | number[] }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

const config = useVisualizationConfig()

// TODO [sb]: Consider switching to a global keyboard shortcut handler.
const shortcuts = {
  zoomIn: (e: KeyboardEvent) => (e.ctrlKey || e.metaKey) && e.key === 'z',
  showAll: (e: KeyboardEvent) => (e.ctrlKey || e.metaKey) && e.key === 'a',
}

const LABEL_FONT_STYLE = '10px DejaVuSansMonoBook'
const POINT_LABEL_PADDING_X_PX = 7
const POINT_LABEL_PADDING_Y_PX = 2
const ANIMATION_DURATION_MS = 400
const VISIBLE_POINTS = 'visible'
const DEFAULT_LIMIT = 1024
const ACCENT_COLOR: Color = { red: 78, green: 165, blue: 253 }
const SIZE_SCALE_MULTIPLER = 100
const FILL_COLOR = `rgba(${ACCENT_COLOR.red * 255},${ACCENT_COLOR.green * 255},${
  ACCENT_COLOR.blue * 255
},0.8)`

const ZOOM_EXTENT = [0.5, 20] satisfies BrushSelection
const RIGHT_BUTTON = 2
const MID_BUTTON = 1
const MID_BUTTON_CLICKED = 4
const SCROLL_WHEEL = 0

const SHAPE_TO_SYMBOL: Record<string, SymbolType> = {
  cross: d3.symbolCross,
  diamond: d3.symbolDiamond,
  square: d3.symbolSquare,
  star: d3.symbolStar,
  triangle: d3.symbolTriangle,
}

const SCALE_TO_D3_SCALE: Record<ScaleType, () => ScaleContinuousNumeric<number, number>> = {
  [ScaleType.Linear]: () => d3.scaleLinear(),
  [ScaleType.Logarithmic]: () => d3.scaleLog(),
}

const data = computed<Data>(() => {
  let rawData = props.data
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
    x: { label: '', scale: ScaleType.Linear },
    y: { label: '', scale: ScaleType.Linear },
  }
  const points = rawData.points ?? { labels: 'visible' }
  const focus: Focus | undefined = rawData.focus
  return { axis, points, data, focus }
})

const containerNode = ref<HTMLElement>()
const pointsNode = ref<SVGGElement>()
const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()
const zoomNode = ref<SVGGElement>()
const brushNode = ref<SVGGElement>()

const d3Points = computed(() => d3.select(pointsNode.value))
const d3XAxis = computed(() => d3.select(xAxisNode.value))
const d3YAxis = computed(() => d3.select(yAxisNode.value))
const d3Zoom = computed(() => d3.select(zoomNode.value))
const d3Brush = computed(() => d3.select(brushNode.value))

const bounds = ref<[number, number, number, number]>()
const brushExtent = ref<BrushSelection>()
const limit = ref(DEFAULT_LIMIT)
const focus = ref<Focus>()
const shouldAnimate = ref(false)
const xDomain = ref<[min: number, max: number]>([0, 1])
const yDomain = ref<[min: number, max: number]>([0, 1])

const xScale = computed(() =>
  axisD3Scale(data.value.axis.x).domain(xDomain.value).range([0, boxWidth.value]),
)
const yScale = computed(() =>
  axisD3Scale(data.value.axis.y).domain(yDomain.value).range([boxHeight.value, 0]),
)

const symbol: Symbol<unknown, Point> = d3.symbol()

const animationDuration = computed(() => (shouldAnimate.value ? ANIMATION_DURATION_MS : 0))
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
const width = ref(Math.max(config.value.width ?? 0, config.value.nodeSize.x))
watchPostEffect(() => {
  width.value = config.value.fullscreen
    ? containerNode.value?.parentElement?.clientWidth ?? 0
    : Math.max(config.value.width ?? 0, config.value.nodeSize.x)
})
const height = ref(config.value.height ?? (config.value.nodeSize.x * 3) / 4)
watchPostEffect(() => {
  height.value = config.value.fullscreen
    ? containerNode.value?.parentElement?.clientHeight ?? 0
    : config.value.height ?? (config.value.nodeSize.x * 3) / 4
})
const boxWidth = computed(() => Math.max(0, width.value - margin.value.left - margin.value.right))
const boxHeight = computed(() => Math.max(0, height.value - margin.value.top - margin.value.bottom))
const xTicks = computed(() => boxWidth.value / 40)
const yTicks = computed(() => boxHeight.value / 20)
const xLabelLeft = computed(
  () =>
    margin.value.left +
    boxWidth.value / 2 -
    getTextWidth(data.value.axis.x.label, LABEL_FONT_STYLE) / 2,
)
const xLabelTop = computed(() => boxHeight.value + margin.value.top + 20)
const yLabelLeft = computed(
  () => -boxHeight.value / 2 + getTextWidth(data.value.axis.y.label, LABEL_FONT_STYLE) / 2,
)
const yLabelTop = computed(() => -margin.value.left + 15)

function updatePreprocessor() {
  emit(
    'update:preprocessor',
    'Standard.Visualization.Scatter_Plot',
    'process_to_json_text',
    bounds.value == null ? 'Nothing' : '[' + bounds.value.join(',') + ']',
    limit.value.toString(),
  )
}

onMounted(updatePreprocessor)

watchEffect(() => (focus.value = data.value.focus))

/**
 * Helper function calculating extreme values and paddings to make sure data will fit nicely.
 *
 * It traverses through data getting minimal and maximal values, and calculates padding based on
 * span calculated from above values, multiplied by 10% so that the plot is a little bit smaller
 * than the container.
 */
const extremesAndDeltas = computed(() => {
  const [xMin = 0, xMax = 0] = d3.extent(data.value.data, (point) => point.x)
  const [yMin = 0, yMax = 0] = d3.extent(data.value.data, (point) => point.y)
  const dx = xMax - xMin
  const dy = yMax - yMin
  const paddingX = 0.1 * dx
  const paddingY = 0.1 * dy
  return { xMin, xMax, yMin, yMax, paddingX, paddingY, dx, dy }
})

let startX = 0
let startY = 0
let actionStartXScale = xScale.value.copy()
let actionStartYScale = yScale.value.copy()

const zoom = computed(() =>
  d3
    .zoom<SVGGElement, unknown>()
    .filter((event: Event) => {
      if (
        event instanceof MouseEvent &&
        event.type === 'mousedown' &&
        (event.button === RIGHT_BUTTON || event.button === MID_BUTTON)
      ) {
        return true
      } else if (
        event instanceof WheelEvent &&
        event.type === 'wheel' &&
        event.button === SCROLL_WHEEL
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
    .scaleExtent(ZOOM_EXTENT)
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('zoom', zoomed)
    .on('start', startZoom),
)
watchEffect(() => d3Zoom.value.call(zoom.value))

/** Helper function called on pan/scroll. */
function zoomed(event: D3ZoomEvent<Element, unknown>) {
  shouldAnimate.value = false
  const xScale_ = xScale.value
  const yScale_ = yScale.value

  function innerRescale(distanceScale: d3.ZoomTransform) {
    xDomain.value = distanceScale.rescaleX(xScale_).domain()
    yDomain.value = distanceScale.rescaleY(yScale_).domain()
  }

  function getScaleForZoom(scale: number) {
    return d3.zoomIdentity
      .translate(startX - margin.value.left, startY - margin.value.top)
      .scale(scale)
      .translate(-startX + margin.value.left, -startY + margin.value.top)
  }

  if (event.sourceEvent instanceof MouseEvent && event.sourceEvent.buttons === RIGHT_BUTTON) {
    xScale_.domain(actionStartXScale.domain())
    yScale_.domain(actionStartYScale.domain())
    const rmbDivider = 100
    const zoomAmount = rmbZoomValue(event.sourceEvent) / rmbDivider
    const distanceScale = getScaleForZoom(Math.exp(zoomAmount))
    innerRescale(distanceScale)
  } else if (event.sourceEvent instanceof WheelEvent) {
    if (event.sourceEvent.ctrlKey) {
      const pinchDivider = 100
      const zoomAmount = -event.sourceEvent.deltaY / pinchDivider
      const distanceScale = getScaleForZoom(Math.exp(zoomAmount))
      innerRescale(distanceScale)
    } else {
      const distanceScale = d3.zoomIdentity.translate(
        -event.sourceEvent.deltaX,
        -event.sourceEvent.deltaY,
      )
      innerRescale(distanceScale)
    }
  } else if (
    event.sourceEvent instanceof MouseEvent &&
    event.sourceEvent.buttons === MID_BUTTON_CLICKED
  ) {
    const movementFactor = 2
    const distanceScale = d3.zoomIdentity.translate(
      event.sourceEvent.movementX / movementFactor,
      event.sourceEvent.movementY / movementFactor,
    )
    innerRescale(distanceScale)
  } else {
    innerRescale(event.transform)
  }
}

/**
 * Return the zoom value computed from the initial right-mouse-button event to the current
 * right-mouse event.
 */
function rmbZoomValue(event: MouseEvent | WheelEvent | undefined) {
  const dX = (event?.offsetX ?? 0) - startX
  const dY = (event?.offsetY ?? 0) - startY
  return dX - dY
}

/** Helper function called when starting to pan/scroll. */
function startZoom(event: D3ZoomEvent<Element, unknown>) {
  startX = event.sourceEvent?.offsetX ?? 0
  startY = event.sourceEvent?.offsetY ?? 0
  actionStartXScale = xScale.value.copy()
  actionStartYScale = yScale.value.copy()
}

const brush = computed(() =>
  d3
    .brush()
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('start brush', (event: D3BrushEvent<unknown>) => {
      brushExtent.value = event.selection ?? undefined
    }),
)
watchEffect(() => d3Brush.value.call(brush.value))

/** Zoom into the selected area of the plot.
 *
 * Based on https://www.d3-graph-gallery.com/graph/interactivity_brush.html
 * Section "Brushing for zooming". */
function zoomToSelected() {
  shouldAnimate.value = true
  focus.value = undefined
  if (
    brushExtent.value == null ||
    !Array.isArray(brushExtent.value[0]) ||
    !Array.isArray(brushExtent.value[1])
  ) {
    return
  }
  const xScale_ = xScale.value
  const yScale_ = yScale.value
  const [[xMinRaw, yMaxRaw], [xMaxRaw, yMinRaw]] = brushExtent.value
  const xMin = xScale_.invert(xMinRaw)
  const xMax = xScale_.invert(xMaxRaw)
  const yMin = yScale_.invert(yMinRaw)
  const yMax = yScale_.invert(yMaxRaw)
  bounds.value = [xMin, yMin, xMax, yMax]
  updatePreprocessor()
  xDomain.value = [xMin, xMax]
  yDomain.value = [yMin, yMax]
}

useEventConditional(
  document,
  'keydown',
  () => brushExtent.value != null,
  (event) => {
    if (shortcuts.zoomIn(event)) {
      zoomToSelected()
      endBrushing()
    }
  },
)

watch([boxWidth, boxHeight], () => (shouldAnimate.value = false))

/** Helper function to match a d3 shape from its name. */
function matchShape(d: Point) {
  return d.shape != null ? SHAPE_TO_SYMBOL[d.shape] ?? d3.symbolCircle : d3.symbolCircle
}

/** Construct either a linear or a logarithmic D3 scale.
 *
 * The scale kind is selected depending on update contents.
 *
 * @param axis Axis information as received in the visualization update.
 * @returns D3 scale. */
function axisD3Scale(axis: AxisConfiguration | undefined) {
  return axis != null ? SCALE_TO_D3_SCALE[axis.scale]() : d3.scaleLinear()
}

watchEffect(() => {
  // Update the axes in d3.
  const { xMin, xMax, yMin, yMax, paddingX, paddingY, dx, dy } = extremesAndDeltas.value
  const focus_ = focus.value
  if (focus_?.x != null && focus_.y != null && focus_.zoom != null) {
    const newPaddingX = dx * (1 / (2 * focus_.zoom))
    const newPaddingY = dy * (1 / (2 * focus_.zoom))
    xDomain.value = [focus_.x - newPaddingX, focus_.x + newPaddingX]
    yDomain.value = [focus_.y - newPaddingY, focus_.y + newPaddingY]
  } else {
    xDomain.value = [xMin - paddingX, xMax + paddingX]
    yDomain.value = [yMin - paddingY, yMax + paddingY]
  }
})

// ==============
// === Update ===
// ==============

// === Update x axis ===

watchPostEffect(() =>
  d3XAxis.value
    .transition()
    .duration(animationDuration.value)
    .call(d3.axisBottom(xScale.value).ticks(xTicks.value)),
)

// === Update y axis ===

watchPostEffect(() =>
  d3YAxis.value
    .transition()
    .duration(animationDuration.value)
    .call(d3.axisLeft(yScale.value).ticks(yTicks.value)),
)

// === Update contents ===

watchPostEffect(() => {
  const xScale_ = xScale.value
  const yScale_ = yScale.value
  d3Points.value
    .selectAll<SVGPathElement, unknown>('path')
    .data(data.value.data)
    .join((enter) => enter.append('path'))
    .transition()
    .duration(animationDuration.value)
    .attr(
      'd',
      symbol.type(matchShape).size((d) => (d.size ?? 1.0) * SIZE_SCALE_MULTIPLER),
    )
    .style('fill', (d) => d.color ?? FILL_COLOR)
    .attr('transform', (d) => `translate(${xScale_(d.x)}, ${yScale_(d.y)})`)
  if (data.value.points.labels === VISIBLE_POINTS) {
    d3Points.value
      .selectAll<SVGPathElement, unknown>('text')
      .data(data.value.data)
      .join((enter) => enter.append('text').attr('class', 'label'))
      .transition()
      .duration(animationDuration.value)
      .text((d) => d.label ?? '')
      .attr('x', (d) => xScale_(d.x) + POINT_LABEL_PADDING_X_PX)
      .attr('y', (d) => yScale_(d.y) + POINT_LABEL_PADDING_Y_PX)
  }
})

// ======================
// === Event handlers ===
// ======================

function fitAll() {
  shouldAnimate.value = true
  focus.value = undefined
  bounds.value = undefined
  limit.value = DEFAULT_LIMIT
  xDomain.value = [
    extremesAndDeltas.value.xMin - extremesAndDeltas.value.paddingX,
    extremesAndDeltas.value.xMax + extremesAndDeltas.value.paddingX,
  ]
  yDomain.value = [
    extremesAndDeltas.value.yMin - extremesAndDeltas.value.paddingY,
    extremesAndDeltas.value.yMax + extremesAndDeltas.value.paddingY,
  ]
  updatePreprocessor()
}

function endBrushing() {
  brushExtent.value = undefined
  d3Brush.value.call(brush.value.move, null)
}

useEvent(document, 'keydown', (event) => {
  if (shortcuts.showAll(event)) {
    fitAll()
  }
})
useEvent(document, 'click', endBrushing)
useEvent(document, 'auxclick', endBrushing)
useEvent(document, 'contextmenu', endBrushing)
useEvent(document, 'scroll', endBrushing)
</script>

<template>
  <VisualizationContainer :below-toolbar="true">
    <template #toolbar>
      <button class="image-button active">
        <img :src="ShowAllIcon" alt="Fit all" @click="fitAll" />
      </button>
      <button class="image-button" :class="{ active: brushExtent != null }">
        <img :src="FindIcon" alt="Zoom to selected" @click="zoomToSelected" />
      </button>
    </template>
    <div ref="containerNode" class="ScatterplotVisualization" @pointerdown.stop>
      <svg :width="width" :height="height">
        <g :transform="`translate(${margin.left}, ${margin.top})`">
          <defs>
            <clipPath id="clip">
              <rect :width="boxWidth" :height="boxHeight"></rect>
            </clipPath>
          </defs>
          <g ref="xAxisNode" class="axis-x" :transform="`translate(0, ${boxHeight})`"></g>
          <g ref="yAxisNode" class="axis-y"></g>
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
          <g ref="zoomNode" class="zoom" :width="boxWidth" :height="boxHeight" fill="none">
            <g ref="brushNode" class="brush"></g>
          </g>
        </g>
      </svg>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://fonts.cdnfonts.com/css/dejavu-sans-mono');

.ScatterplotVisualization {
  user-select: none;
  display: flex;
}

.ScatterplotVisualization .selection {
  rx: 4px;
  stroke: transparent;
}

.label-y {
  transform: rotate(-90deg);
}
</style>
