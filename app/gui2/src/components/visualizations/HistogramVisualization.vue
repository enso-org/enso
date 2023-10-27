<script lang="ts">
import { defineKeybinds } from '@/util/shortcuts'

export const name = 'Histogram'
export const inputType =
  'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector | Standard.Image.Data.Histogram.Histogram'
export const defaultPreprocessor = [
  'Standard.Visualization.Histogram',
  'process_to_json_text',
] as const

const bindings = defineKeybinds('histogram-visualization', {
  zoomIn: ['Mod+Z'],
  showAll: ['Mod+A'],
})

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
  Linear = 'linear',
  Logarithmic = 'logarithmic',
}

interface AxisConfiguration {
  label?: string
  scale: ScaleType
}

interface AxesConfiguration {
  x: AxisConfiguration | undefined
  y: AxisConfiguration | undefined
}

interface Bin {
  x0: number | undefined
  x1: number | undefined
  length: number | undefined
}
</script>

<script setup lang="ts">
import { computed, ref, watch, watchEffect, watchPostEffect } from 'vue'

import * as d3 from 'd3'

import SvgIcon from '@/components/SvgIcon.vue'
import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig'

import { useEvent, useEventConditional } from '@/util/events'
import { getTextWidth } from '@/util/measurement'

const MARGIN = 25
const AXIS_LABEL_HEIGHT = 10
const ANIMATION_DURATION_MS = 400
const DEFAULT_NUMBER_OF_BINS = 50
const COLOR_LEGEND_WIDTH = 5
const DEFAULT_AXES_CONFIGURATION: AxesConfiguration = {
  x: { scale: ScaleType.Linear },
  y: { scale: ScaleType.Linear },
}
const RMB_DIVIDER = 100
const PINCH_DIVIDER = 100

const EPSILON = 0.001
const ZOOM_EXTENT = [0.5, 20] satisfies d3.BrushSelection
const RIGHT_BUTTON = 2
const MID_BUTTON = 1
const MID_BUTTON_CLICKED = 4
const SCROLL_WHEEL = 0

const props = defineProps<{ data: Data }>()

const config = useVisualizationConfig()

const containerNode = ref<HTMLElement>()
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
const brushExtent = ref<d3.BrushSelection>()
const zoomLevel = ref(1)
const shouldAnimate = ref(false)

const xDomain = ref([0, 1])
const yDomain = ref([0, 1])

const isBrushing = computed(() => brushExtent.value != null)
// The maximum value MUST NOT be 0, otherwise 0 will be in the middle of the y axis.
const yMax = computed(() => d3.max(bins.value, (d) => d.length) || 1)
const originalXScale = computed(() =>
  d3.scaleLinear().range([0, boxWidth.value]).domain(xExtents.value),
)
const xScale = computed(() => d3.scaleLinear().domain(xDomain.value).range([0, boxWidth.value]))
const yScale = computed(() => d3.scaleLinear().domain(yDomain.value).range([boxHeight.value, 0]))
const fill = computed(() =>
  d3.scaleSequential().domain(yDomain.value).interpolator(d3.interpolateViridis),
)
const yAxis = computed(() => {
  const yTicks = yScale.value.ticks().filter(Number.isInteger)
  return d3.axisLeft(yScale.value).tickFormat(d3.format('d')).tickValues(yTicks)
})

const animationDuration = ref(() => (shouldAnimate.value ? ANIMATION_DURATION_MS : 0))
const bins = computed<Bin[]>(() => {
  if (rawBins.value != null) {
    return rawBins.value.map((length, i) => ({ x0: i, x1: i + 1, length }))
  } else if (points.value != null) {
    const dataDomain = originalXScale.value.domain()
    const histogram = d3
      .bin()
      .domain([dataDomain[0] ?? 0, dataDomain[1] ?? 1])
      .thresholds(originalXScale.value.ticks(binCount.value))
    return histogram(points.value)
  } else {
    return []
  }
})

watchEffect(() => {
  let rawData = props.data
  if (rawData == null) {
    console.error('Histogram was not passed any data.')
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
const width = ref(Math.max(config.width ?? 0, config.nodeSize.x))
watchPostEffect(() => {
  width.value = config.fullscreen
    ? containerNode.value?.parentElement?.clientWidth ?? 0
    : Math.max(config.width ?? 0, config.nodeSize.x)
})
const height = ref(config.height ?? (config.nodeSize.x * 3) / 4)
watchPostEffect(() => {
  height.value = config.fullscreen
    ? containerNode.value?.parentElement?.clientHeight ?? 0
    : config.height ?? (config.nodeSize.x * 3) / 4
})
const boxWidth = computed(() => Math.max(0, width.value - margin.value.left - margin.value.right))
const boxHeight = computed(() => Math.max(0, height.value - margin.value.top - margin.value.bottom))
const xLabelTop = computed(() => boxHeight.value + margin.value.bottom - AXIS_LABEL_HEIGHT / 2)
const xLabelLeft = computed(() => boxWidth.value / 2 + getTextWidth(axis.value.x?.label) / 2)
const yLabelTop = computed(() => -margin.value.left + AXIS_LABEL_HEIGHT)
const yLabelLeft = computed(() => -boxHeight.value / 2 + getTextWidth(axis.value.y?.label) / 2)

let startX = 0
let startY = 0
let startClientX = 0
let startClientY = 0
let actionStartXScale = xScale.value.copy()
let actionStartZoomLevel = zoomLevel.value

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
function zoomed(event: d3.D3ZoomEvent<Element, unknown>) {
  shouldAnimate.value = false
  const xScale_ = xScale.value
  const yScale_ = yScale.value

  function innerRescale(transformEvent: d3.ZoomTransform) {
    xDomain.value = transformEvent.rescaleX(xScale_).domain()
    const newYDomain = transformEvent.rescaleY(yScale_).domain()
    const yMin = newYDomain[0] ?? 0
    if (yMin >= -EPSILON && yMin <= EPSILON) {
      newYDomain[0] -= yMin
      newYDomain[1] -= yMin
    }
    if ((newYDomain[0] ?? 0) >= 0) {
      yDomain.value = newYDomain
    }
  }

  if (event.sourceEvent instanceof MouseEvent && event.sourceEvent.buttons === RIGHT_BUTTON) {
    const zoomAmount = rmbZoomValue(event.sourceEvent) / RMB_DIVIDER
    const scale = Math.exp(zoomAmount)
    const distanceScale = getScaleForZoom(scale)
    xScale_.domain(actionStartXScale.domain())
    xDomain.value = distanceScale.rescaleX(xScale_).domain()
    zoomLevel.value = actionStartZoomLevel * scale
  } else if (event.sourceEvent instanceof WheelEvent) {
    if (event.sourceEvent.ctrlKey) {
      const zoomAmount = -event.sourceEvent.deltaY / PINCH_DIVIDER
      const scale = Math.exp(zoomAmount)
      const distanceScale = getScaleForZoom(scale)
      xDomain.value = distanceScale.rescaleX(xScale_).domain()
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
}

/** Return the zoom value computed from the initial right-mouse-button event to the current
 * right-mouse event. */
function rmbZoomValue(event: MouseEvent | WheelEvent | undefined) {
  const dX = (event?.clientX ?? 0) - startClientX
  const dY = (event?.clientY ?? 0) - startClientY
  return dX - dY
}

/** Helper function called when starting to pan/scroll. */
function startZoom(event: d3.D3ZoomEvent<Element, unknown>) {
  startX = event.sourceEvent?.offsetX ?? 0
  startY = event.sourceEvent?.offsetY ?? 0
  startClientX = event.sourceEvent?.clientX ?? 0
  startClientY = event.sourceEvent?.clientY ?? 0
  actionStartXScale = xScale.value.copy()
  actionStartZoomLevel = zoomLevel.value
}

const brush = computed(() =>
  d3
    .brushX()
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('start brush', (event: d3.D3BrushEvent<unknown>) => {
      brushExtent.value = event.selection ?? undefined
    }),
)
// Note: The brush element must be a child of the zoom element - this is only way we found to have
// both zoom and brush events working at the same time. See https://stackoverflow.com/a/59757276.
watchEffect(() => d3Brush.value.call(brush.value))

/** Zoom into the selected area of the plot.
 *
 * Based on https://www.d3-graph-gallery.com/graph/interactivity_brush.html
 * Section "Brushing for zooming". */
function zoomToSelected() {
  if (brushExtent.value == null) {
    return
  }
  focus.value = undefined
  const xScale_ = xScale.value
  const startRaw = brushExtent.value[0]
  const endRaw = brushExtent.value[1]
  const start = typeof startRaw === 'number' ? startRaw : startRaw[0]
  const end = typeof endRaw === 'number' ? endRaw : endRaw[0]
  const selectionWidth = end - start
  zoomLevel.value *= boxWidth.value / selectionWidth
  const xMin = xScale_.invert(start)
  const xMax = xScale_.invert(end)
  xDomain.value = [xMin, xMax]
  shouldAnimate.value = true
}

function endBrushing() {
  brushExtent.value = undefined
  d3Brush.value.call(brush.value.move, null)
}

function zoomIn() {
  zoomToSelected()
  endBrushing()
}

useEventConditional(document, 'keydown', isBrushing, bindings.handler({ zoomIn }))

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

const xExtents = computed<[min: number, max: number]>(() => {
  const extremesAndDeltas_ = extremesAndDeltas.value
  return [
    extremesAndDeltas_.xMin - extremesAndDeltas_.paddingX,
    extremesAndDeltas_.xMax + extremesAndDeltas_.paddingX,
  ]
})

watchEffect(() => {
  const focus_ = focus.value
  if (focus_?.x != null && focus_.zoom != null) {
    let paddingX = extremesAndDeltas.value.dx / (2 * focus_.zoom)
    xDomain.value = [focus_.x - paddingX, focus_.x + paddingX]
  } else {
    xDomain.value = xExtents.value
  }
})

/**
 * Update height of the color legend to match the height of the canvas.
 * Set up `stop` attributes on color legend gradient to match `colorScale`, so color legend shows correct colors
 * used by histogram.
 */
function updateColorLegend(colorScale: d3.ScaleSequential<string>) {
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

watch([boxWidth, boxHeight], () => {
  shouldAnimate.value = false
  queueMicrotask(endBrushing)
})

// === Update x axis ===

watchPostEffect(() =>
  d3XAxis.value
    .transition()
    .duration(animationDuration.value)
    .call(d3.axisBottom(xScale.value).ticks(width.value / 40)),
)

// === Update y axis ===

watchEffect(() => {
  if (yDomain.value[0] === 0 && yDomain.value[1] !== yMax.value) {
    shouldAnimate.value = true
    yDomain.value = [0, yMax.value]
  }
})

watchPostEffect(() =>
  d3YAxis.value.transition().duration(animationDuration.value).call(yAxis.value),
)

// === Update contents ===

watchPostEffect(() => {
  const originalXScale_ = originalXScale.value
  const xScale_ = xScale.value
  const yScale_ = yScale.value
  const boxHeight_ = boxHeight.value
  const fill_ = fill.value
  const zoomLevel_ = zoomLevel.value
  updateColorLegend(fill_)

  d3Plot.value
    .selectAll('rect')
    .data(bins.value)
    .join((enter) => enter.append('rect').attr('x', 1))
    .transition()
    .duration(animationDuration.value)
    .attr(
      'transform',
      (d) => `translate(${xScale_(d.x0 ?? 0)}, ${yScale_(d.length ?? 0)}) scale(${zoomLevel_}, 1)`,
    )
    .attr('width', (d) => originalXScale_(d.x1 ?? 0) - originalXScale_(d.x0 ?? 0))
    .attr('height', (d) => Math.max(0, boxHeight_ - yScale_(d.length ?? 0)))
    .style('fill', (d) => fill_(d.length ?? 0))
})

// ======================
// === Event handlers ===
// ======================

function showAll() {
  focus.value = undefined
  zoomLevel.value = 1
  xDomain.value = originalXScale.value.domain()
  shouldAnimate.value = true
  endBrushing()
}

useEvent(document, 'keydown', bindings.handler({ showAll }))
useEvent(document, 'click', endBrushing)
useEvent(document, 'auxclick', endBrushing)
useEvent(document, 'contextmenu', endBrushing)
useEvent(document, 'scroll', endBrushing)
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <template #toolbar>
      <button class="image-button active">
        <SvgIcon name="show_all" alt="Fit all" @click="showAll" />
      </button>
      <button class="image-button" :class="{ active: brushExtent != null }">
        <SvgIcon name="find" alt="Zoom to selected" @click="zoomToSelected" />
      </button>
    </template>
    <div ref="containerNode" class="HistogramVisualization" @pointerdown.stop>
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
            v-if="axis.x?.label"
            class="label label-x"
            text-anchor="end"
            :x="xLabelLeft"
            :y="xLabelTop"
            v-text="axis.x?.label"
          ></text>
          <text
            v-if="axis.y?.label"
            class="label label-y"
            text-anchor="end"
            :x="yLabelLeft"
            :y="yLabelTop"
            v-text="axis.y?.label"
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
