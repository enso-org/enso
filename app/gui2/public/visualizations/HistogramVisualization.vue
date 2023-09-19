<script lang="ts">
export const name = 'Histogram'
export const inputType =
  'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector | Standard.Image.Data.Histogram.Histogram'

// eslint-disable-next-line no-redeclare
declare const d3: typeof import('d3')

/**
 * A d3.js histogram visualization.
 *
 *
 * Data format (JSON):
 * {
 *   "axis" : {
 *      "x" : { "label" : "x-axis label", "scale" : "linear" },
 *      "y" : { "label" : "y-axis label", "scale" : "logarithmic" },
 *   },
 *   "focus" : { "x" : 1.7, "y" : 2.1, "zoom" : 3.0 },
 *   "color" : "rgb(1.0,0.0,0.0)",
 *   "bins"  : 10,
 *   "data"  : {
 *      "values" : [0.1, 0.2, 0.1, 0.15, 0.7],
 *   }
 * }
 */

type Data = HistogramData | HistogramArrayData | HistogramJSONData | HistogramUpdate

interface HistogramInnerData {
  values: number[]
  bins: number[] | undefined
}

interface HistogramData {
  update: undefined
  data: HistogramInnerData
  json: undefined
  axis: AxesConfiguration
  focus: Focus | undefined
  bins: number | undefined
}

type HistogramArrayData = number[] & {
  update: undefined
  data: undefined
  json: undefined
  axis: undefined
  focus: undefined
  bins: undefined
}

interface HistogramJSONData {
  update: undefined
  data: undefined
  json: number[]
  axis: undefined
  focus: undefined
  bins: undefined
}

interface HistogramUpdate {
  update: 'diff'
  data: HistogramInnerData | undefined
  json: undefined
  axis: undefined
  focus: undefined
  bins: undefined
}

interface Focus {
  x: number
  y: number
  zoom: number
}

enum ScaleType {
  linear = 'linear',
  logarithmic = 'logarithmic',
}

interface AxisConfiguration {
  label?: string
  scale: ScaleType
}

interface AxesConfiguration {
  x: AxisConfiguration
  y: AxisConfiguration
}

type Scale = ScaleContinuousNumeric<number, number>

interface Bin {
  x0: number | undefined
  x1: number | undefined
  length: number | undefined
}
</script>

<script setup lang="ts">
import ShowAllIcon from './icons/show_all.svg'
import FindIcon from './icons/find.svg'

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7.8.5/+esm'
import type { ScaleContinuousNumeric } from 'd3'

import VisualizationContainer from 'builtins/VisualizationContainer.vue'
import { useVisualizationConfig } from 'builtins/useVisualizationConfig.ts'

import { useEvent } from './events.ts'
import { getTextWidth } from './measurement.ts'

import { computed, onMounted, ref, watch, watchEffect } from 'vue'
import type { BrushSelection } from 'd3'
import type { D3ZoomEvent } from 'd3'
import type { ZoomTransform } from 'd3'
import type { D3BrushEvent } from 'd3'
import type { ScaleSequential } from 'd3'

const shortcuts = {
  zoomIn: (e: KeyboardEvent) => (e.ctrlKey || e.metaKey) && e.key === 'z',
  showAll: (e: KeyboardEvent) => (e.ctrlKey || e.metaKey) && e.key === 'a',
}

const MARGIN = 25
const AXIS_LABEL_HEIGHT = 10
const ANIMATION_DURATION = 1000
const DEFAULT_NUMBER_OF_BINS = 50
const COLOR_LEGEND_WIDTH = 5
const DEFAULT_AXES_CONFIGURATION: AxesConfiguration = {
  x: { scale: ScaleType.linear },
  y: { scale: ScaleType.linear },
}
const RMB_DIVIDER = 100
const PINCH_DIVIDER = 100

const EPSILON = 0.001
const MIN_SCALE = 0.5
const MAX_SCALE = 20
const ZOOM_EXTENT = [MIN_SCALE, MAX_SCALE] satisfies BrushSelection
const RIGHT_BUTTON = 2
const MID_BUTTON = 1
const MID_BUTTON_CLICKED = 4
const SCROLL_WHEEL = 0

const props = defineProps<{ data: Data | string }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

const config = useVisualizationConfig()

const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()
const plotNode = ref<SVGGElement>()
const colorLegendGradientNode = ref<SVGElement>()
const zoomNode = ref<SVGGElement>()
const brushNode = ref<SVGGElement>()

const d3XAxis = computed(() => d3.select(xAxisNode.value))
const d3YAxis = computed(() => d3.select(yAxisNode.value))
const d3Plot = computed(() => d3.select(plotNode.value))
const d3ColorLegendGradient = computed(() => d3.select(colorLegendGradientNode.value))
const d3Zoom = computed(() => d3.select(zoomNode.value))
const d3Brush = computed(() => d3.select(brushNode.value))

const points = ref<number[]>([])
const rawBins = ref<number[]>()
const binCount = ref(DEFAULT_NUMBER_OF_BINS)
const axis = ref(DEFAULT_AXES_CONFIGURATION)
const focus = ref<Focus>()
const brushExtent = ref<BrushSelection>()
const zoomLevel = ref(1)
const maxY = ref(1)

const dataScale = ref(d3.scaleLinear())
const xScale = ref(d3.scaleLinear())
const yScale = ref(d3.scaleLinear())
const fill = ref(d3.scaleSequential().interpolator(d3.interpolateViridis))
const yAxis = ref(d3.axisLeft(yScale.value).tickFormat(d3.format('d')))
watchEffect(() => yAxis.value.scale(yScale.value))

watchEffect(() => {
  let rawData: Data | undefined =
    typeof props.data === 'string' ? JSON.parse(props.data) : props.data
  if (rawData == null) {
    console.error('Heatmap was not passed any data.')
  } else {
    const isUpdate = rawData.update === 'diff'
    let newData: HistogramInnerData | number[] = []
    if (Array.isArray(rawData)) {
      newData = rawData
      rawData = {} as Data
    }
    if (isUpdate) {
      if (rawData.data != null) {
        newData = rawData.data
      }
    } else if (rawData.data != null) {
      newData = rawData.data
    } else if (Array.isArray(rawData)) {
      newData = rawData
    } else if (rawData.json != null && Array.isArray(rawData.json)) {
      newData = rawData.json
    } else {
      newData = []
    }

    if (rawData.axis != null) {
      axis.value = rawData.axis
    }
    if (rawData.focus != null) {
      focus.value = rawData.focus
    }
    if (rawData.bins != null) {
      binCount.value = Math.max(1, rawData.bins)
    }
    if (!isUpdate) {
      rawBins.value = rawData.data?.bins ?? undefined
    }

    const values = Array.isArray(newData) ? newData : newData?.values ?? []
    points.value = values.filter((value) => typeof value === 'number' && !Number.isNaN(value))
  }
})

// =================
// === Positions ===
// =================

const margin = computed(() => ({
  top: MARGIN / 2.0,
  right: MARGIN / 2.0,
  bottom: MARGIN + (axis.value?.x?.label ? AXIS_LABEL_HEIGHT : 0),
  left: MARGIN + (axis.value?.y?.label ? AXIS_LABEL_HEIGHT : 0),
}))
const width = computed(() => Math.max(config.value.width ?? 0, config.value.nodeSize.x) ?? 100)
const height = computed(() => config.value.height ?? (config.value.nodeSize.x * 3) / 4)
const boxWidth = computed(() => Math.max(0, width.value - margin.value.left - margin.value.right))
const boxHeight = computed(() => Math.max(0, height.value - margin.value.top - margin.value.bottom))
const xLabelTop = computed(() => boxHeight.value + margin.value.bottom - AXIS_LABEL_HEIGHT / 2)
const xLabelLeft = computed(() => boxWidth.value / 2 + getTextWidth(axis.value.x.label) / 2)
const yLabelTop = computed(() => -margin.value.left + AXIS_LABEL_HEIGHT)
const yLabelLeft = computed(() => -boxHeight.value / 2 + getTextWidth(axis.value.y.label) / 2)

let startX = 0
let startY = 0

function getScaleForZoom(scale: number) {
  return d3.zoomIdentity
    .translate(startX - (AXIS_LABEL_HEIGHT + MARGIN), startY - MARGIN)
    .scale(scale)
    .translate(-startX + (AXIS_LABEL_HEIGHT + MARGIN), -startY + MARGIN)
}

/** Initialise panning and zooming functionality on the visualization. */
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
  const xScale_ = xScale.value
  const yScale_ = yScale.value

  function innerRescale(transformEvent: ZoomTransform) {
    xScale_.domain(transformEvent.rescaleX(xScale_).domain())
    const newYDomain = transformEvent.rescaleY(yScale_).domain()
    const yMin = newYDomain[0] ?? 0
    if (yMin >= -EPSILON && yMin <= EPSILON) {
      newYDomain[0] -= yMin
      newYDomain[1] -= yMin
    }
    if ((newYDomain[0] ?? 0) >= 0) {
      yScale_.domain(newYDomain)
    }
  }

  if (event.sourceEvent instanceof MouseEvent && event.sourceEvent.buttons === RIGHT_BUTTON) {
    xScale_.domain(dataScale.value.domain())
    const zoomAmount = rmbZoomValue(event.sourceEvent) / RMB_DIVIDER
    const scale = Math.exp(zoomAmount)
    const distanceScale = getScaleForZoom(scale)
    xScale_.domain(distanceScale.rescaleX(xScale_).domain())
    zoomLevel.value = scale
  } else if (event.sourceEvent instanceof WheelEvent) {
    if (event.sourceEvent.ctrlKey) {
      const zoomAmount = -event.sourceEvent.deltaY / PINCH_DIVIDER
      const scale = Math.exp(zoomAmount)
      const distanceScale = getScaleForZoom(scale)
      xScale_.domain(distanceScale.rescaleX(xScale_).domain())
      zoomLevel.value *= scale
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
  rescale(xScale_, yScale_, false)
}

/** Return the zoom value computed from the initial right-mouse-button event to the current
 * right-mouse event. */
function rmbZoomValue(event: MouseEvent | WheelEvent | undefined) {
  const dX = (event?.offsetX ?? 0) - startX
  const dY = (event?.offsetY ?? 0) - startY
  return dX - dY
}

/** Helper function called when starting to pan/scroll. */
function startZoom(event: D3ZoomEvent<Element, unknown>) {
  startX = event.sourceEvent?.offsetX ?? 0
  startY = event.sourceEvent?.offsetY ?? 0
}

const brush = computed(() =>
  d3
    .brushX()
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('start brush', (event: D3BrushEvent<unknown>) => {
      brushExtent.value = event.selection ?? undefined
    }),
)
// Note: The brush element must be a child of the zoom element - this is only way we found to have
// both zoom and brush events working at the same time. See https://stackoverflow.com/a/59757276.
watchEffect(() => d3Brush.value.call(brush.value))

/** Zooms into selected fragment of plot.
 *
 * Based on https://www.d3-graph-gallery.com/graph/interactivity_brush.html
 * Section "Brushing for zooming". */
function zoomIn() {
  if (brushExtent.value == null) {
    return
  }
  focus.value = undefined
  const xScale_ = xScale.value
  const startRaw = brushExtent.value[0]
  const endRaw = brushExtent.value[1]
  const start = typeof startRaw === 'number' ? startRaw : startRaw[0]
  const end = typeof endRaw === 'number' ? endRaw : endRaw[0]
  const xMin = xScale_.invert(start)
  const xMax = xScale_.invert(end)
  xScale_.domain([xMin, xMax])
  const selectionWidth = end - start
  zoomLevel.value *= boxWidth.value / selectionWidth
  rescale(xScale_, yScale.value, true)
}

/**
 * Removes brush, keyboard event and zoom button when end event is captured.
 */
function endBrushing() {
  brushExtent.value = undefined
  d3Brush.value.call(brush.value.move, null)
}

watch(
  () => brushExtent.value != null,
  (conditionMet, _, onCleanup) => {
    if (conditionMet) {
      function handler(event: KeyboardEvent) {
        if (shortcuts.zoomIn(event)) {
          zoomIn()
          endBrushing()
        }
      }
      document.addEventListener('keydown', handler)
      onCleanup(() => document.removeEventListener('keydown', handler))
    }
  },
)

/** Animate the chart to a new scale. */
function rescale(xScale: Scale, yScale: Scale, shouldAnimate: boolean) {
  const duration = shouldAnimate ? ANIMATION_DURATION : 0.0
  d3XAxis.value
    .transition()
    .duration(duration)
    .call(d3.axisBottom(xScale).ticks(width.value / 40))
  const yTicks = yScale.ticks().filter(Number.isInteger)
  d3YAxis.value
    .transition()
    .duration(duration)
    .call(
      d3
        .axisLeft(yScale)
        .ticks(height.value / 20)
        .tickValues(yTicks)
        .tickFormat(d3.format('d')),
    )
  d3Plot.value
    .selectAll<SVGRectElement, Bin>('rect')
    .transition()
    .duration(duration)
    .attr(
      'transform',
      (d) =>
        `translate(${xScale(d.x0 ?? 0)}, ${yScale(d.length ?? 0)}) scale(${zoomLevel.value}, 1)`,
    )
  update()
}

/**
 * Return the extrema of the data and and paddings that ensure data will fit into the
 * drawing area.
 *
 * It traverses through data getting minimal and maximal values, and calculates padding based on
 * span calculated from above values, multiplied by 10% so that the plot is a little bit smaller
 * than the container.
 */
const extremesAndDeltas = computed(() => {
  let xMin = rawBins.value != null ? 0 : Math.min(...points.value)
  let xMax = rawBins.value != null ? rawBins.value.length - 1 : Math.max(...points.value)
  const dx = xMax - xMin
  const binCount_ = rawBins.value?.length || binCount.value
  const paddingX = Math.max(0.1 * dx, 1 / binCount_)
  return { xMin, xMax, paddingX, dx }
})

watchEffect(() => {
  // Update the x-axis in D3.
  const focus_ = focus.value
  const extremesAndDeltas_ = extremesAndDeltas.value
  let domainX: [min: number, max: number]
  if (focus_?.x != null && focus_.zoom != null) {
    let paddingX = extremesAndDeltas_.dx / (2 * focus_.zoom)
    domainX = [focus_.x - paddingX, focus_.x + paddingX]
  } else {
    domainX = [
      extremesAndDeltas_.xMin - extremesAndDeltas_.paddingX,
      extremesAndDeltas_.xMax + extremesAndDeltas_.paddingX,
    ]
  }
  dataScale.value.domain(domainX).range([0, boxWidth.value])
  xScale.value.domain(domainX).range([0, boxWidth.value])
  d3XAxis.value.call(d3.axisBottom(xScale.value).ticks(width.value / 40))
})

watchEffect(() => {
  // Update the y-axis in D3.
  const yScale_ = yScale.value
  yScale_.range([boxHeight.value, 0])
  if (yScale_.domain()[0] === 0) {
    yScale_.domain([0, maxY.value])
  }
  fill.value.domain(yScale_.domain())
  const yTicks = yScale_.ticks().filter(Number.isInteger)
  yAxis.value.scale(yScale_).tickValues(yTicks)
  d3YAxis.value.call(yAxis.value)
})

/** Update the d3 histogram with the current data.
 *
 * Bind the new data to the plot, creating new bars, removing old ones and
 * update the axes accordingly. */
function updateHistogram(
  bins: Bin[],
  dataScale: Scale,
  yScale: Scale,
  fill: ScaleSequential<string>,
  boxHeight: number,
) {
  updateColorLegend(fill)
  const items = d3Plot.value.selectAll<SVGRectElement, Bin>('rect').data(bins)
  items.join(
    (enter) => enter.append('rect').attr('x', 1),
    (update) =>
      update
        .attr('transform', (d) => `translate(${dataScale(d.x0 ?? 0)}, ${yScale(d.length ?? 0)})`)
        .attr('width', (d) => dataScale(d.x1 ?? 0) - dataScale(d.x0 ?? 0))
        .attr('height', (d) => Math.max(0, boxHeight - yScale(d.length ?? 0)))
        .style('fill', (d) => fill(d.length ?? 0)),
  )
}

/**
 * Update height of the color legend to match the height of the canvas.
 * Set up `stop` attributes on color legend gradient to match `colorScale`, so color legend shows correct colors
 * used by histogram.
 */
function updateColorLegend(colorScale: ScaleSequential<string>) {
  const colorScaleToGradient = (t: number, i: number, n: number[]) => ({
    offset: `${(100 * i) / n.length}%`,
    color: colorScale(t),
  })
  d3ColorLegendGradient.value
    .selectAll('stop')
    .data(colorScale.ticks().map(colorScaleToGradient))
    .enter()
    .append('stop')
    .attr('offset', (d) => d.offset)
    .attr('stop-color', (d) => d.color)
}

// ==============
// === Update ===
// ==============

function update() {
  let bins: Bin[] = []
  if (rawBins.value != null) {
    bins = rawBins.value.map((length, i) => ({ x0: i, x1: i + 1, length }))
  } else if (points.value != null) {
    const dataDomain = dataScale.value.domain()
    const histogram = d3
      .bin()
      .domain([dataDomain[0] ?? 0, dataDomain[1] ?? 1])
      .thresholds(dataScale.value.ticks(binCount.value))
    bins = histogram(points.value)
  }
  // The maximum value MUST NOT be 0, otherwise 0 will be in the middle of the y axis.
  maxY.value = d3.max(bins, (d) => d.length) || 1
  updateHistogram(bins, dataScale.value, yScale.value, fill.value, boxHeight.value)
}

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Histogram', 'process_to_json_text')
  update()
})

watch([width, height, maxY], () => queueMicrotask(update))
watchEffect(update)

watch([width, height], () => queueMicrotask(endBrushing))

// ===============
// === Zooming ===
// ===============

function fitAll() {
  focus.value = undefined
  zoomLevel.value = 1
  xScale.value.domain(dataScale.value.domain())
  queueMicrotask(update)
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
        <img :src="FindIcon" alt="Zoom to selected" @click="zoomIn" />
      </button>
    </template>
    <div class="HistogramVisualization" @pointerdown.stop>
      <svg :width="width" :height="height">
        <rect
          class="color-legend"
          :width="COLOR_LEGEND_WIDTH"
          :height="boxHeight"
          :transform="`translate(${margin.left - COLOR_LEGEND_WIDTH}, ${margin.top})`"
          :style="{ fill: 'url(#color-legend-gradient)' }"
        />
        <g :transform="`translate(${margin.left}, ${margin.top})`">
          <defs>
            <clipPath id="histogram-clip-path">
              <rect :width="boxWidth" :height="boxHeight"></rect>
            </clipPath>
            <linearGradient
              id="color-legend-gradient"
              ref="colorLegendGradientNode"
              x1="0%"
              y1="100%"
              x2="0%"
              y2="0%"
            ></linearGradient>
          </defs>
          <g ref="xAxisNode" class="axis-x" :transform="`translate(0, ${boxHeight})`"></g>
          <g ref="yAxisNode" class="axis-y"></g>
          <text
            v-if="axis.x.label"
            class="label label-x"
            text-anchor="end"
            :x="xLabelLeft"
            :y="xLabelTop"
            v-text="axis.x.label"
          ></text>
          <text
            v-if="axis.y.label"
            class="label label-y"
            text-anchor="end"
            :x="yLabelLeft"
            :y="yLabelTop"
            v-text="axis.y.label"
          ></text>
          <g ref="plotNode" clip-path="url(#histogram-clip-path)"></g>
          <g ref="zoomNode" class="zoom">
            <g ref="brushNode" class="brush"></g>
          </g>
        </g>
      </svg>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
.HistogramVisualization {
  user-select: none;
  display: flex;
}

.HistogramVisualization .selection {
  rx: 4px;
  stroke: transparent;
}

.label-y {
  transform: rotate(-90deg);
}
</style>
