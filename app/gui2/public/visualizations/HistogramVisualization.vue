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

enum Scale {
  linear = 'linear',
  logarithmic = 'logarithmic',
}

interface AxisConfiguration {
  label?: string
  scale: Scale
}

interface AxesConfiguration {
  x: AxisConfiguration
  y: AxisConfiguration
}

interface Bin {
  x0: number | undefined
  x1: number | undefined
  length: number | undefined
}

interface ScaleConfiguration {
  x: d3Types.ScaleContinuousNumeric<number, number>
  y: d3Types.ScaleContinuousNumeric<number, number>
  zoom: number
}
</script>

<script setup lang="ts">
import ShowAllIcon from './icons/show_all.svg'
import FindIcon from './icons/find.svg'

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7.8.5/+esm'
import type * as d3Types from 'd3'

import VisualizationContainer from 'builtins/VisualizationContainer.vue'
import { useVisualizationConfig } from 'builtins/useVisualizationConfig.ts'

import { getTextWidth } from './measurement.ts'

import { computed, onMounted, onUnmounted, ref, watch, watchEffect } from 'vue'

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
  x: { scale: Scale.linear },
  y: { scale: Scale.linear },
}

const MIN_SCALE = 0.5
const MAX_SCALE = 20
const RIGHT_BUTTON = 2
const MID_BUTTON = 1
const MID_BUTTON_CLICKED = 4
const SCROLL_WHEEL = 0

const props = defineProps<{ data: Data | string }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

const config = useVisualizationConfig()

const containerNode = ref<HTMLElement>()
const rootNode = ref<SVGGElement>()
const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()
const colorLegendGradientNode = ref<SVGElement>()
const plotNode = ref<SVGGElement>()
const zoomNode = ref<SVGGElement>()
const brushNode = ref<SVGGElement>()

const points = ref<number[]>([])
const rawBins = ref<number[]>()
const binCount = ref(DEFAULT_NUMBER_OF_BINS)
const axis = ref(DEFAULT_AXES_CONFIGURATION)
const focus = ref<Focus>()
const brushExtent = ref<d3Types.BrushSelection>()
/** This is INCORRECT, but SAFE, as long as {@link updateHistogram} is called before
 * {@link updatePanAndZoom}. */
const scale = ref<ScaleConfiguration>(null!)
const maxY = ref(1)

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

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Histogram', 'process_to_json_text')
})

let zoom = {} as ReturnType<typeof updatePanAndZoom>

// =================
// === Positions ===
// =================

const margin = computed(() => ({
  top: MARGIN / 2.0,
  right: MARGIN / 2.0,
  bottom: MARGIN + (axis.value?.x?.label ? AXIS_LABEL_HEIGHT : 0),
  left: MARGIN + (axis.value?.y?.label ? AXIS_LABEL_HEIGHT : 0),
}))
const width = computed(
  () =>
    config.value.width ??
    config.value.nodeSize.x ??
    containerNode.value?.getBoundingClientRect().width ??
    100,
)
const height = computed(
  () =>
    config.value.height ?? ((containerNode.value?.getBoundingClientRect().width ?? 100) * 3) / 4,
)
const boxWidth = computed(() => Math.max(0, width.value - margin.value.left - margin.value.right))
const boxHeight = computed(() => Math.max(0, height.value - margin.value.top - margin.value.bottom))
const xLabelTop = computed(() => boxHeight.value + margin.value.bottom - AXIS_LABEL_HEIGHT / 2)
const xLabelLeft = computed(() => boxWidth.value / 2 + getTextWidth(axis.value.x.label) / 2)
const yLabelTop = computed(() => -margin.value.left + AXIS_LABEL_HEIGHT)
const yLabelLeft = computed(() => -boxHeight.value / 2 + getTextWidth(axis.value.y.label) / 2)

// =============
// === Setup ===
// =============

function update(
  rawBins: number[] | undefined,
  points: number[] | undefined,
  width: number,
  height: number,
) {
  let bins: Bin[] = []
  if (rawBins != null) {
    bins = rawBins.map((length, i) => ({ x0: i, x1: i + 1, length }))
  } else if (points != null) {
    const xScale_ = xScale.value
    const xDomain = xScale_.domain()
    const histogram = d3
      .bin()
      .domain([xDomain[0] ?? 0, xDomain[1] ?? 1])
      .thresholds(xScale_.ticks(binCount.value))
    bins = histogram(points)
  }
  // The maximum value MUST NOT be 0, otherwise 0 will be in the middle of the y axis.
  maxY.value = d3.max(bins, (d) => d.length) || 1
  updateHistogram(bins)
  zoom = updatePanAndZoom()
  // Note: The brush element must be a child of the zoom element - this is only way we found to have
  // both zoom and brush events working at the same time. See https://stackoverflow.com/a/59757276.
  d3.select(brushNode.value).call(brush.value)
}

onMounted(() => {
  update(rawBins.value, points.value, width.value, height.value)
})

watch(
  () => [rawBins.value, points.value, width.value, height.value] as const,
  ([bins, points, width, height]) => {
    update(bins, points, width, height)
  },
)

/**
 * Initialise panning and zooming functionality on the visualization.
 */
function updatePanAndZoom() {
  const extent = [MIN_SCALE, MAX_SCALE] satisfies d3Types.BrushSelection
  let startX = 0
  let startY = 0
  const zoom = d3
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
    .scaleExtent(extent)
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('zoom', zoomed)
    .on('start', startZoom)

  let transformedScale = { ...scale.value }
  let tempRmbScale = { ...scale.value }

  /**
   * Helper function called on pan/scroll.
   */
  function zoomed(event: d3Types.D3ZoomEvent<Element, unknown>) {
    function innerRescale(transformEvent: d3Types.ZoomTransform) {
      transformedScale.x = transformEvent.rescaleX(transformedScale.x)
      if ((transformEvent.rescaleY(transformedScale.y).domain()[0] ?? 0) >= 0) {
        transformedScale.y = transformEvent.rescaleY(transformedScale.y)
      }
    }

    function getScaleForZoom(scale: number) {
      return d3.zoomIdentity
        .translate(startX - (AXIS_LABEL_HEIGHT + MARGIN), startY - MARGIN)
        .scale(scale)
        .translate(-startX + (AXIS_LABEL_HEIGHT + MARGIN), -startY + MARGIN)
    }

    if (event.sourceEvent instanceof MouseEvent && event.sourceEvent.buttons === RIGHT_BUTTON) {
      transformedScale.x = tempRmbScale.x
      const rmbDivider = 100.0
      const zoomAmount = rmbZoomValue(event.sourceEvent) / rmbDivider
      const scale = Math.exp(zoomAmount)
      const distanceScale = getScaleForZoom(scale)
      transformedScale.x = distanceScale.rescaleX(transformedScale.x)
      transformedScale.zoom = tempRmbScale.zoom * scale
    } else if (event.sourceEvent instanceof WheelEvent) {
      if (event.sourceEvent.ctrlKey) {
        const pinchDivider = 100.0
        const zoomAmount = -event.sourceEvent.deltaY / pinchDivider
        const scale = Math.exp(zoomAmount)
        const distanceScale = getScaleForZoom(scale)
        transformedScale.x = distanceScale.rescaleX(transformedScale.x)
        transformedScale.zoom = transformedScale.zoom * scale
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

    rescale(transformedScale, false)
  }

  /**
   * Return the position of this event in local canvas coordinates.
   */
  function getPos(event: MouseEvent | undefined) {
    return { x: event?.offsetX ?? 0, y: event?.offsetY ?? 0 }
  }

  /**
   * Return the zoom value computed from the initial right-mouse-button event to the current
   * right-mouse event.
   */
  function rmbZoomValue(event: MouseEvent | WheelEvent | undefined) {
    const end = getPos(event)
    const dX = (event?.offsetX ?? 0) - startX
    const dY = (event?.offsetY ?? 0) - startY
    return dX - dY
  }

  /**
   * Helper function called when starting to pan/scroll.
   */
  function startZoom(event: d3Types.D3ZoomEvent<Element, unknown>) {
    startX = event.sourceEvent?.offsetX ?? 0
    startY = event.sourceEvent?.offsetY ?? 0
    tempRmbScale = { ...transformedScale }
  }

  return { zoom, transformedScale }
}

const brush = computed(() =>
  d3
    .brushX()
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('start brush', (event: d3Types.D3BrushEvent<unknown>) => {
      brushExtent.value = event.selection ?? undefined
    }),
)

/**
 * Zooms into selected fragment of plot.
 *
 * Based on https://www.d3-graph-gallery.com/graph/interactivity_brush.html
 * Section "Brushing for zooming".
 */
function zoomIn() {
  focus.value = undefined
  if (brushExtent.value == null) {
    return
  }
  const startRaw = brushExtent.value[0]
  const endRaw = brushExtent.value[1]
  const start = typeof startRaw === 'number' ? startRaw : startRaw[0]
  const end = typeof endRaw === 'number' ? endRaw : endRaw[0]
  const xMin = zoom.transformedScale.x.invert(start)
  const xMax = zoom.transformedScale.x.invert(end)
  zoom.transformedScale.x.domain([xMin, xMax])
  const dx = end - start
  zoom.transformedScale.zoom = zoom.transformedScale.zoom * (boxWidth.value / dx)
  rescale(zoom.transformedScale, true)
}

/**
 * Removes brush, keyboard event and zoom button when end event is captured.
 */
function endBrushing() {
  brushExtent.value = undefined
  d3.select(brushNode.value).call(brush.value.move, null)
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

/**
 * Helper function for rescaling the data points with a new scale.
 */
function rescale(scale: ScaleConfiguration, shouldAnimate: boolean) {
  const duration = shouldAnimate ? ANIMATION_DURATION : 0.0
  d3.select(xAxisNode.value)
    .transition()
    .duration(duration)
    .call(d3.axisBottom(scale.x).ticks(width.value / 40))
  d3.select(yAxisNode.value)
    .transition()
    .duration(duration)
    .call(d3.axisLeft(scale.y).ticks(height.value / 20))
  d3.select(plotNode.value)
    .selectAll<SVGRectElement, Bin>('rect')
    .transition()
    .duration(duration)
    .attr(
      'transform',
      (d) => `translate(${scale.x(d.x0 ?? 0)}, ${scale.y(d.length ?? 0)}) scale(${scale.zoom}, 1)`,
    )
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
  const paddingX = 0.1 * dx
  return { xMin, xMax, paddingX, dx }
})

const xScale = computed(() => {
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
  return d3.scaleLinear().domain(domainX).range([0, boxWidth.value])
})

const yScale = computed(() => d3.scaleLinear().range([boxHeight.value, 0]).domain([0, maxY.value]))
const fill = computed(() =>
  d3.scaleSequential().interpolator(d3.interpolateViridis).domain(yScale.value.domain()),
)
const yAxisTicks = computed(() => yScale.value.ticks().filter(Number.isInteger))
const yAxis = computed(() =>
  d3.axisLeft(yScale.value).tickValues(yAxisTicks.value).tickFormat(d3.format('d')),
)

/** Update the d3 histogram with the current data.
 *
 * Bind the new data to the plot, creating new bars, removing old ones and
 * update the axes accordingly. */
function updateHistogram(bins: Bin[]) {
  const xScale_ = xScale.value
  d3.select(xAxisNode.value).call(d3.axisBottom(xScale_).ticks(width.value / 40))
  const yScale_ = yScale.value
  d3.select(yAxisNode.value).call(yAxis.value)
  const fill_ = fill.value
  updateColorLegend(fill_)
  if (plotNode.value == null) {
    console.error('Histogram could not find the HTML element for the chart.')
  } else {
    const items = d3.select(plotNode.value).selectAll<SVGRectElement, Bin>('rect').data(bins)
    const boxHeight_ = boxHeight.value
    items.join(
      (enter) => enter.append('rect').attr('x', 1),
      (update) =>
        update
          .attr(
            'transform',
            (d) => 'translate(' + xScale_(d.x0 ?? 0) + ',' + yScale_(d.length ?? 0) + ')',
          )
          .attr('width', (d) => xScale_(d.x1 ?? 0) - xScale_(d.x0 ?? 0))
          .attr('height', (d) => boxHeight_ - yScale_(d.length ?? 0))
          .style('fill', (d) => fill_(d.length ?? 0)),
    )
  }
  scale.value = { x: xScale_, y: yScale_, zoom: 1.0 }
}

/**
 * Update height of the color legend to match the height of the canvas.
 * Set up `stop` attributes on color legend gradient to match `colorScale`, so color legend shows correct colors
 * used by histogram.
 */
function updateColorLegend(colorScale: d3Types.ScaleSequential<string>) {
  const colorScaleToGradient = (t: number, i: number, n: number[]) => ({
    offset: `${(100 * i) / n.length}%`,
    color: colorScale(t),
  })
  d3.select(colorLegendGradientNode.value)
    .selectAll('stop')
    .data(colorScale.ticks().map(colorScaleToGradient))
    .enter()
    .append('stop')
    .attr('offset', (d) => d.offset)
    .attr('stop-color', (d) => d.color)
}

function fitAll() {
  focus.value = undefined
  d3.select(zoomNode.value).transition().duration(0).call(zoom.zoom.transform, d3.zoomIdentity)
  const extremesAndDeltas_ = extremesAndDeltas.value
  let domainX = [
    extremesAndDeltas_.xMin - extremesAndDeltas_.paddingX,
    extremesAndDeltas_.xMax + extremesAndDeltas_.paddingX,
  ]
  zoom.transformedScale.x.domain(domainX)
  zoom.transformedScale.zoom = 1
  rescale(zoom.transformedScale, true)
}

function onKeydown(event: KeyboardEvent) {
  if (shortcuts.showAll(event)) {
    fitAll()
  }
}

onMounted(() => {
  document.addEventListener('keydown', onKeydown)
  document.addEventListener('click', endBrushing)
  document.addEventListener('auxclick', endBrushing)
  document.addEventListener('contextmenu', endBrushing)
  document.addEventListener('scroll', endBrushing)
})

onUnmounted(() => {
  document.removeEventListener('keydown', onKeydown)
  document.removeEventListener('click', endBrushing)
  document.removeEventListener('auxclick', endBrushing)
  document.removeEventListener('contextmenu', endBrushing)
  document.removeEventListener('scroll', endBrushing)
})
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
    <div ref="containerNode" class="HistogramVisualization">
      <svg :width="width" :height="height">
        <rect
          class="color-legend"
          :width="COLOR_LEGEND_WIDTH"
          :height="boxHeight"
          :transform="`translate(${margin.left - COLOR_LEGEND_WIDTH}, ${margin.top})`"
          :style="{ fill: 'url(#color-legend-gradient)' }"
        />
        <g ref="rootNode" :transform="`translate(${margin.left}, ${margin.top})`">
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
          <g ref="zoomNode" class="zoom" :width="boxWidth" :height="boxHeight" fill="none">
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
