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
  linear = 'linear',
  logarithmic = 'logarithmic',
}

interface AxisConfiguration {
  label: string
  scale: ScaleType
}

interface AxesConfiguration {
  x: AxisConfiguration
  y: AxisConfiguration
}

type Scale = ScaleContinuousNumeric<number, number>

interface Color {
  red: number
  green: number
  blue: number
}
</script>

<script setup lang="ts">
import ShowAllIcon from './icons/show_all.svg'
import FindIcon from './icons/find.svg'

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7.8.5/+esm'
import type {} from 'd3'

import { useEvent } from './events.ts'
import { getTextWidth } from './measurement.ts'

import VisualizationContainer from 'builtins/VisualizationContainer.vue'
import { useVisualizationConfig } from 'builtins/useVisualizationConfig.ts'

import { computed, onMounted, ref, watch, watchEffect } from 'vue'
import type { SymbolType } from 'd3'
import type { ScaleContinuousNumeric } from 'd3'
import type { Selection } from 'd3'
import type { BrushSelection } from 'd3'
import type { D3ZoomEvent } from 'd3'
import type { D3BrushEvent } from 'd3'

const props = defineProps<{ data: Partial<Data> | number[] | string }>()
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

const MIN_SCALE = 0.5
const MAX_SCALE = 20
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

const SCALE_TO_D3_SCALE: Record<ScaleType, ScaleContinuousNumeric<number, number>> = {
  [ScaleType.linear]: d3.scaleLinear(),
  [ScaleType.logarithmic]: d3.scaleLog(),
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
    x: { label: '', scale: ScaleType.linear },
    y: { label: '', scale: ScaleType.linear },
  }
  const points = rawData.points ?? { labels: 'visible' }
  const focus: Focus | undefined = rawData.focus
  return { axis, points, data, focus }
})

const pointsNode = ref<SVGGElement>()
const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()
const zoomNode = ref<SVGGElement>()
const brushNode = ref<SVGGElement>()

const d3Points = computed(() => d3.select(pointsNode.value))
const d3Brush = computed(() => d3.select(brushNode.value))
const d3XAxis = computed(() => d3.select(xAxisNode.value))
const d3YAxis = computed(() => d3.select(yAxisNode.value))

const d3Labels = ref<Selection<SVGTextElement, Point, SVGGElement, unknown>>()
const bounds = ref<[number, number, number, number]>()
const brushExtent = ref<BrushSelection>()
const limit = ref(DEFAULT_LIMIT)
const focus = ref<Focus>()

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
const width = computed(() => Math.max(config.value.width ?? 0, config.value.nodeSize.x) ?? 100)
const height = computed(() => config.value.height ?? (config.value.nodeSize.x * 3) / 4)
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

onMounted(() => {
  updatePreprocessor()
})

watchEffect(() => {
  focus.value = data.value.focus
})

let scaleAndAxis = {} as ReturnType<typeof updateAxes>
let zoom = {} as ReturnType<typeof addPanAndZoom>

function update() {
  scaleAndAxis = updateAxes()
  zoom = addPanAndZoom()
  redrawPoints()
  updateBrushing()
}

onMounted(update)
watch([data, width, height], update)

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

/**
 * Adds panning and zooming functionality to the visualization.
 */
function addPanAndZoom() {
  const extent: [min: number, max: number] = [MIN_SCALE, MAX_SCALE]
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

  let transformedScale = { ...scaleAndAxis }
  let tempRmbScale = { ...scaleAndAxis }

  /**
   * Helper function called on pan/scroll.
   */
  function zoomed(event: D3ZoomEvent<Element, unknown>) {
    function rescale(distanceScale: d3.ZoomTransform) {
      transformedScale.xScale = distanceScale.rescaleX(transformedScale.xScale)
      transformedScale.yScale = distanceScale.rescaleY(transformedScale.yScale)
    }

    function getScaleForZoom(scale: number) {
      return d3.zoomIdentity
        .translate(startX - margin.value.left, startY - margin.value.top)
        .scale(scale)
        .translate(-startX + margin.value.left, -startY + margin.value.top)
    }

    if (event.sourceEvent instanceof MouseEvent && event.sourceEvent.buttons === RIGHT_BUTTON) {
      transformedScale.xScale = tempRmbScale.xScale
      transformedScale.yScale = tempRmbScale.yScale
      const rmbDivider = 100
      const zoomAmount = rmbZoomValue(event.sourceEvent) / rmbDivider
      const distanceScale = getScaleForZoom(Math.exp(zoomAmount))
      rescale(distanceScale)
    } else if (event.sourceEvent instanceof WheelEvent) {
      if (event.sourceEvent.ctrlKey) {
        const pinchDivider = 100
        const zoomAmount = -event.sourceEvent.deltaY / pinchDivider
        const distanceScale = getScaleForZoom(Math.exp(zoomAmount))
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
      event.sourceEvent.buttons === MID_BUTTON_CLICKED
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
    d3Points.value
      .selectAll<SVGPathElement, Point>('path')
      .attr(
        'transform',
        (d) => `translate(${transformedScale.xScale(d.x)}, ${transformedScale.yScale(d.y)})`,
      )

    if (data.value.points.labels === VISIBLE_POINTS) {
      d3Points.value
        .selectAll<SVGTextElement, Point>('text')
        .attr('x', (d) => transformedScale.xScale(d.x) + POINT_LABEL_PADDING_X_PX)
        .attr('y', (d) => transformedScale.yScale(d.y) + POINT_LABEL_PADDING_Y_PX)
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
  function rmbZoomValue(event: MouseEvent | WheelEvent | undefined) {
    const dX = (event?.offsetX ?? 0) - startX
    const dY = (event?.offsetY ?? 0) - startY
    return dX - dY
  }

  /**
   * Helper function called when starting to pan/scroll.
   */
  function startZoom(event: D3ZoomEvent<Element, unknown>) {
    startX = event.sourceEvent?.offsetX ?? 0
    startY = event.sourceEvent?.offsetY ?? 0
    tempRmbScale = { ...transformedScale }
  }

  return { zoom, transformedScale }
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

/**
 * Add brushing functionality to the plot.
 *
 * The brush is a tool which enables user to select points, and zoom into the selection using
 * a keyboard shortcut or button event.
 */
function updateBrushing() {
  // The brush element must be a child of zoom element - this is only way we found to have both
  // zoom and brush events working at the same time. See https://stackoverflow.com/a/59757276
  d3Brush.value.call(brush.value)
}

/**
 * Zooms into the selected fragment of the plot.
 *
 * Based on https://www.d3-graph-gallery.com/graph/interactivity_brush.html
 * Section "Brushing for zooming".
 */
function zoomToSelected() {
  focus.value = undefined
  if (
    brushExtent.value == null ||
    !Array.isArray(brushExtent.value[0]) ||
    !Array.isArray(brushExtent.value[1])
  ) {
    return
  }
  const [[xMinRaw, yMaxRaw], [xMaxRaw, yMinRaw]] = brushExtent.value
  brushExtent.value = undefined
  let xMin = zoom.transformedScale.xScale.invert(xMinRaw)
  let xMax = zoom.transformedScale.xScale.invert(xMaxRaw)
  let yMin = zoom.transformedScale.yScale.invert(yMinRaw)
  let yMax = zoom.transformedScale.yScale.invert(yMaxRaw)

  bounds.value = [xMin, yMin, xMax, yMax]
  updatePreprocessor()

  zoom.transformedScale.xScale.domain([xMin, xMax])
  zoom.transformedScale.yScale.domain([yMin, yMax])

  zoomingHelper(zoom.transformedScale.xScale, zoom.transformedScale.yScale)
}

watch(
  () => brushExtent.value != null,
  (conditionMet, _, onCleanup) => {
    if (conditionMet) {
      function handler(event: KeyboardEvent) {
        if (shortcuts.zoomIn(event)) {
          zoomToSelected()
          endBrushing()
        }
      }
      document.addEventListener('keydown', handler)
      onCleanup(() => document.removeEventListener('keydown', handler))
    }
  },
)

/**
 * Helper function for zooming in after the scale has been updated.
 */
function zoomingHelper(xScale: Scale, yScale: Scale) {
  d3XAxis.value
    .transition()
    .duration(ANIMATION_DURATION_MS)
    .call(d3.axisBottom(xScale).ticks(xTicks.value))
  d3YAxis.value
    .transition()
    .duration(ANIMATION_DURATION_MS)
    .call(d3.axisLeft(yScale).ticks(yTicks.value))

  d3Points.value
    .selectAll<SVGPathElement, Point>('path')
    .transition()
    .duration(ANIMATION_DURATION_MS)
    .attr('transform', (d) => `translate(${xScale(d.x)}, ${yScale(d.y)})`)

  if (data.value.points.labels === VISIBLE_POINTS) {
    d3Points.value
      .selectAll<SVGTextElement, Point>('text')
      .transition()
      .duration(ANIMATION_DURATION_MS)
      .attr('x', (d) => xScale(d.x) + POINT_LABEL_PADDING_X_PX)
      .attr('y', (d) => yScale(d.y) + POINT_LABEL_PADDING_Y_PX)
  }
}

const SIZE_SCALE_MULTIPLER = 100
const FILL_COLOR = `rgba(${ACCENT_COLOR.red * 255},${ACCENT_COLOR.green * 255},${
  ACCENT_COLOR.blue * 255
},0.8)`

watchEffect(() => {
  const { xScale, yScale } = scaleAndAxis
  const symbol = d3.symbol()
  d3Points.value
    .selectAll<SVGPathElement, unknown>('dataPoint')
    .data(data.value.data)
    .join(
      (enter) =>
        enter
          .append('path')
          .attr(
            'd',
            symbol.type(matchShape).size((d: Point) => (d.size ?? 1.0) * SIZE_SCALE_MULTIPLER),
          )
          .style('fill', (d) => d.color ?? FILL_COLOR)
          .attr('transform', (d) => `translate(${xScale(d.x)}, ${yScale(d.y)})`),
      (update) => update.attr('x', (d) => xScale(d.x)).attr('y', (d) => yScale(d.y)),
    )
  if (data.value.points.labels === VISIBLE_POINTS) {
    d3Points.value
      .selectAll('dataPoint')
      .data(data.value.data)
      .join(
        (enter) =>
          enter
            .append('text')
            .attr('class', 'label')
            .text((d) => d.label ?? '')
            .attr('x', (d) => xScale(d.x) + POINT_LABEL_PADDING_X_PX)
            .attr('y', (d) => yScale(d.y) + POINT_LABEL_PADDING_Y_PX),
        (update) =>
          update
            .text((d) => d.label ?? '')
            .attr('x', (d) => xScale(d.x) + POINT_LABEL_PADDING_X_PX)
            .attr('y', (d) => yScale(d.y) + POINT_LABEL_PADDING_Y_PX),
      )
  }
})

/**
 * Create a plot object and populate it with the given data.
 */
function redrawPoints() {
  const { xScale, yScale } = scaleAndAxis
  d3Points.value
    .selectAll<SVGPathElement, Point>('dataPoint')
    .attr('transform', (d) => `translate(${xScale(d.x)}, ${yScale(d.y)})`)
  if (data.value.points.labels === VISIBLE_POINTS) {
    d3Points.value
      .selectAll<SVGPathElement, Point>('dataPoint')
      .attr('x', (d) => xScale(d.x) + POINT_LABEL_PADDING_X_PX)
      .attr('y', (d) => yScale(d.y) + POINT_LABEL_PADDING_Y_PX)
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

  const focus_ = focus.value
  if (focus_ != null) {
    if (focus_.x != null && focus_.y != null && focus_.zoom != null) {
      let newPaddingX = extremesAndDeltas.value.dx * (1 / (2 * focus_.zoom))
      let newPaddingY = extremesAndDeltas.value.dy * (1 / (2 * focus_.zoom))
      domainX = [focus_.x - newPaddingX, focus_.x + newPaddingX]
      domainY = [focus_.y - newPaddingY, focus_.y + newPaddingY]
    }
  }
  return { x: domainX, y: domainY }
})

/**
 * Creates plot's axes.
 */
function updateAxes() {
  const xScale = axisD3Scale(data.value.axis.x).domain(domains.value.x).range([0, boxWidth.value])
  const xAxis = d3.select(xAxisNode.value).call(d3.axisBottom(xScale).ticks(xTicks.value))
  const yScale = axisD3Scale(data.value.axis.y).domain(domains.value.y).range([boxHeight.value, 0])
  const yAxis = d3.select(yAxisNode.value).call(d3.axisLeft(yScale).ticks(yTicks.value))
  return { xScale: xScale, yScale: yScale, xAxis: xAxis, yAxis: yAxis }
}

function fitAll() {
  focus.value = undefined
  if (zoomNode.value == null) {
    console.error('Scatterplot could not find the HTML element for the zoom.')
  } else {
    d3.select(zoomNode.value).transition().duration(0).call(zoom.zoom.transform, d3.zoomIdentity)
  }

  const domainX = [
    extremesAndDeltas.value.xMin - extremesAndDeltas.value.paddingX,
    extremesAndDeltas.value.xMax + extremesAndDeltas.value.paddingX,
  ]
  const domainY = [
    extremesAndDeltas.value.yMin - extremesAndDeltas.value.paddingY,
    extremesAndDeltas.value.yMax + extremesAndDeltas.value.paddingY,
  ]

  zoom.transformedScale.xScale.domain(domainX)
  zoom.transformedScale.yScale.domain(domainY)

  zoomingHelper(zoom.transformedScale.xScale, zoom.transformedScale.yScale)

  bounds.value = undefined
  limit.value = DEFAULT_LIMIT
  updatePreprocessor()
}

function endBrushing() {
  brushExtent.value = undefined
  d3.select(brushNode.value).call(brush.value.move, null)
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
    <div class="ScatterplotVisualization" @pointerdown.stop>
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
