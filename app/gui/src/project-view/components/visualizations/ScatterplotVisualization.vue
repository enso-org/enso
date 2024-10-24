<script lang="ts">
import { useEvent } from '@/composables/events'
import { useVisualizationConfig } from '@/providers/visualizationConfig'
import { Ast } from '@/util/ast'
import { tryNumberToEnso } from '@/util/ast/abstract'
import { Pattern } from '@/util/ast/match'
import { getTextWidthBySizeAndFamily } from '@/util/measurement'
import { defineKeybinds } from '@/util/visualizationBuiltins'
import { computed, ref, watch, watchEffect, watchPostEffect } from 'vue'

export const name = 'Scatter Plot'
export const icon = 'points'
export const inputType = 'Standard.Table.Table.Table | Standard.Base.Data.Vector.Vector'
const DEFAULT_LIMIT = 1024
export const defaultPreprocessor = [
  'Standard.Visualization.Scatter_Plot',
  'process_to_json_text',
  'Nothing',
  DEFAULT_LIMIT.toString(),
] as const

const bindings = defineKeybinds('scatterplot-visualization', {
  zoomToSelected: ['Mod+A'],
})

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
  isTimeSeries: boolean
  x_value_type: string
  is_multi_series: boolean
  get_row_method: string
  error_message: string | null
}

interface Focus {
  x: number
  y: number
  zoom: number
}

interface Point {
  x: number | DateObj | Date
  y: number
  label?: string
  color?: string
  shape?: string
  size?: number
  series?: string
  row_number: number
}

interface PointsConfiguration {
  labels: string
}

enum ScaleType {
  Linear = 'linear',
  Logarithmic = 'logarithmic',
  Time = 'time',
}

interface AxisConfiguration {
  label: string
  scale?: ScaleType
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

interface DateObj {
  day?: number
  month?: number
  year?: number
  hour?: number
  minute?: number
  second?: number
}
</script>

<script setup lang="ts">
const d3 = await import('d3')

const props = defineProps<{ data: Partial<Data> | number[] }>()

const config = useVisualizationConfig()

const LABEL_FONT_STYLE = '10px DejaVuSansMonoBook'
const POINT_LABEL_PADDING_X_PX = 7
const POINT_LABEL_PADDING_Y_PX = 2
const ANIMATION_DURATION_MS = 400
const VISIBLE_POINTS = 'visible'
const ACCENT_COLOR: Color = { red: 78, green: 165, blue: 253 }
const SIZE_SCALE_MULTIPLER = 100
const DEFAULT_FILL_COLOR = `rgba(${ACCENT_COLOR.red},${ACCENT_COLOR.green},${ACCENT_COLOR.blue},0.8)`

const ZOOM_EXTENT = [0.5, 20] satisfies d3.BrushSelection
const RIGHT_BUTTON = 2
const MID_BUTTON = 1
const MID_BUTTON_CLICKED = 4
const SCROLL_WHEEL = 0

const SHAPE_TO_SYMBOL: Record<string, d3.SymbolType> = {
  cross: d3.symbolCross,
  diamond: d3.symbolDiamond,
  square: d3.symbolSquare,
  star: d3.symbolStar,
  triangle: d3.symbolTriangle,
}

const createDateTime = (x: DateObj) => {
  const dateTime = new Date()
  if (x.day != null) dateTime.setDate(x.day)
  if (x.month != null) dateTime.setMonth(x.month)
  if (x.year != null) dateTime.setFullYear(x.year)
  if (x.hour != null) dateTime.setHours(x.hour)
  if (x.minute != null) dateTime.setMinutes(x.minute)
  if (x.second != null) dateTime.setSeconds(x.second)
  return dateTime
}

const data = computed<Data>(() => {
  let rawData = props.data
  const unfilteredData =
    Array.isArray(rawData) ?
      // eslint-disable-next-line camelcase
      rawData.map((y, index) => ({ x: index, y, row_number: index }))
    : rawData.data ?? []
  let data: Point[]
  const isTimeSeries: boolean =
    'x_value_type' in rawData ?
      rawData.x_value_type === 'Time' ||
      rawData.x_value_type === 'Date' ||
      rawData.x_value_type === 'Date_Time'
    : false
  if (isTimeSeries) {
    data = unfilteredData
      .filter((point) => typeof point.y === 'number' && !Number.isNaN(point.y))
      .map((point) => ({ ...point, x: createDateTime(point.x as DateObj) }))
  } else {
    data = unfilteredData.filter(
      (point) =>
        typeof point.x === 'number' &&
        !Number.isNaN(point.x) &&
        typeof point.y === 'number' &&
        !Number.isNaN(point.y),
    )
  }
  if (Array.isArray(rawData)) {
    rawData = {}
  }

  const axis: AxesConfiguration =
    rawData.axis && 'x' in rawData.axis && 'y' in rawData.axis ?
      rawData.axis
    : {
        x: { label: '', scale: isTimeSeries ? ScaleType.Time : ScaleType.Linear },
        y: { label: '', scale: ScaleType.Linear },
      }
  const points = rawData.points ?? { labels: 'visible' }
  const focus: Focus | undefined = rawData.focus
  // eslint-disable-next-line camelcase
  const is_multi_series: boolean = !!rawData.is_multi_series
  // eslint-disable-next-line camelcase
  const get_row_method: string = rawData.get_row_method || 'get_row'
  // eslint-disable-next-line camelcase
  const error_message: string | null = rawData.error_message || null
  return {
    axis,
    points,
    data,
    focus,
    // eslint-disable-next-line camelcase
    is_multi_series,
    // eslint-disable-next-line camelcase
    x_value_type: rawData.x_value_type || '',
    // eslint-disable-next-line camelcase
    get_row_method,
    // eslint-disable-next-line camelcase
    error_message,
    isTimeSeries,
  }
})

const containerNode = ref<HTMLElement>()
const pointsNode = ref<SVGGElement>()
const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()
const zoomNode = ref<SVGGElement>()
const brushNode = ref<SVGGElement>()
const legendNode = ref<SVGGElement>()

const d3Points = computed(() => d3.select(pointsNode.value))
const d3XAxis = computed(() => d3.select(xAxisNode.value))
const d3YAxis = computed(() => d3.select(yAxisNode.value))
const d3Zoom = computed(() => d3.select(zoomNode.value))
const d3Brush = computed(() => d3.select(brushNode.value))
const d3Legend = computed(() => d3.select(legendNode.value))

const bounds = ref<[number, number, number, number]>()
const brushExtent = ref<d3.BrushSelection>()
const limit = ref(DEFAULT_LIMIT)
const focus = ref<Focus>()
const shouldAnimate = ref(false)
const xDomain = ref<number[] | Date[]>([0, 1])
const yDomain = ref<number[] | Date[]>([0, 1])
const selectionEnabled = ref(false)
const createNewFilterNodeEnabled = ref(false)

const isBrushing = computed(() => brushExtent.value != null)

function axisD3Scale(axis: AxisConfiguration | undefined) {
  return axis?.scale === ScaleType.Logarithmic ? d3.scaleLog() : d3.scaleLinear()
}

const xScale = computed(() =>
  axisD3Scale(data.value.axis.x).domain(xDomain.value).range([0, boxWidth.value]),
)
const yScale = computed(() =>
  axisD3Scale(data.value.axis.y).domain(yDomain.value).range([boxHeight.value, 0]),
)

const xScaleTime = computed(() => d3.scaleTime().domain(xDomain.value).range([0, boxWidth.value]))

const symbol: d3.Symbol<unknown, Point> = d3.symbol()

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
const width = computed(() => config.size.x)
const height = computed(() => config.size.y)

const boxWidth = computed(() => Math.max(0, width.value - margin.value.left - margin.value.right))
const boxHeight = computed(() => Math.max(0, height.value - margin.value.top - margin.value.bottom))
const xTicks = computed(() => {
  switch (data.value.x_value_type) {
    case 'Time':
    case 'Date':
    case 'Date_Time':
      return boxWidth.value / 80
    default:
      return boxWidth.value / 40
  }
})

const yTicks = computed(() => boxHeight.value / 20)
const xLabelLeft = computed(
  () =>
    margin.value.left +
    boxWidth.value / 2 -
    getTextWidthBySizeAndFamily(data.value.axis.x.label, LABEL_FONT_STYLE) / 2,
)
const xLabelTop = computed(() => boxHeight.value + margin.value.top + 20)
const yLabelLeft = computed(
  () =>
    -boxHeight.value / 2 +
    getTextWidthBySizeAndFamily(data.value.axis.y.label, LABEL_FONT_STYLE) / 2,
)
const yLabelTop = computed(() => -margin.value.left + 15)
const showYLabelText = computed(() => !data.value.is_multi_series)
const xTickFormat = computed(() => {
  switch (data.value.x_value_type) {
    case 'Time':
      return '%H:%M:%S'
    case 'Date':
      return '%d/%m/%Y'
    default:
      return '%d/%m/%Y %H:%M:%S'
  }
})
const isUsingIndexForX = computed(() => data.value.axis.x.label === 'index')

watchEffect(() => {
  const boundsExpression =
    bounds.value != null ? Ast.Vector.tryBuild(bounds.value, tryNumberToEnso) : undefined
  config.setPreprocessor(
    'Standard.Visualization.Scatter_Plot',
    'process_to_json_text',
    boundsExpression?.code() ?? 'Nothing',
    limit.value.toString(),
  )
})

watchEffect(() => (focus.value = data.value.focus))

/**
 * Helper function calculating extreme values and paddings to make sure data will fit nicely.
 *
 * It traverses through data getting minimal and maximal values, and calculates padding based on
 * span calculated from above values, multiplied by 10% so that the plot is a little bit smaller
 * than the container.
 */
const extremesAndDeltas = computed(() => {
  let yMin = 0
  let yMax = 0
  if (data.value.is_multi_series) {
    const axis = data.value.axis
    const series = Object.keys(axis).filter((s) => s != 'x')
    const allYValues = series.flatMap((s) =>
      data.value.data.map((d) => d[s as keyof Point]),
    ) as number[]
    yMin = Math.min(...allYValues)
    yMax = Math.max(...allYValues)
  } else {
    ;[yMin = 0, yMax = 0] = d3.extent(data.value.data, (point) => point.y)
  }

  const [xMin = 0, xMax = 0] = d3.extent(data.value.data, (point) => point.x as number)
  const dx = Number(xMax) - Number(xMin)
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
        event.deltaMode === 1 ? medDelta
        : event.deltaMode ? maxDelta
        : minDelta
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
function startZoom(event: d3.D3ZoomEvent<Element, unknown>) {
  startX = event.sourceEvent?.offsetX ?? 0
  startY = event.sourceEvent?.offsetY ?? 0
  actionStartXScale = xScale.value.copy()
  actionStartYScale = yScale.value.copy()
}

const brush = computed(() => {
  if (!selectionEnabled.value) {
    return d3.brush().extent([
      [0, 0],
      [0, 0],
    ])
  }

  return d3
    .brush()
    .extent([
      [0, 0],
      [boxWidth.value, boxHeight.value],
    ])
    .on('start brush', (event: d3.D3BrushEvent<unknown>) => {
      brushExtent.value = event.selection ?? undefined
    })
})

watchEffect(() => {
  if (selectionEnabled.value && brush.value) {
    d3Brush.value.call(brush.value)
  } else {
    d3Brush.value.on('.brush', null)
    d3Brush.value.style('cursor', 'default')
    if (brush.value) {
      d3Brush.value.call(brush.value)
    }
  }
})

watch([boxWidth, boxHeight], () => (shouldAnimate.value = false))

/** Helper function to match a d3 shape from its name. */
function matchShape(d: Point) {
  return d.shape != null ? SHAPE_TO_SYMBOL[d.shape] ?? d3.symbolCircle : d3.symbolCircle
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
    xDomain.value = [Number(xMin) - paddingX, Number(xMax) + paddingX]
    yDomain.value = [yMin - paddingY, yMax + paddingY]
  }
})

// ==============
// === Update ===
// ==============

// === Update x axis ===

watchPostEffect(() => {
  const xCallVal =
    data.value.isTimeSeries ?
      d3
        .axisBottom<Date>(xScaleTime.value)
        .ticks(xTicks.value)
        .tickFormat(d3.timeFormat(xTickFormat.value))
    : d3.axisBottom(xScale.value).ticks(xTicks.value)
  return d3XAxis.value.transition().duration(animationDuration.value).call(xCallVal)
})

// === Update y axis ===

watchPostEffect(() =>
  d3YAxis.value
    .transition()
    .duration(animationDuration.value)
    .call(d3.axisLeft(yScale.value).ticks(yTicks.value)),
)

function getPlotData(data: Data) {
  const axis = data.axis
  if (data.is_multi_series) {
    const series = Object.keys(axis).filter((s) => s != 'x')
    const transformedData = series.flatMap((s) =>
      data.data.map((d) => ({
        ...d,
        x: d.x,
        y: d[s as keyof Point],
        series: s,
      })),
    )
    return transformedData
  }
  return data.data
}

const filterPattern = computed(() => Pattern.parse('__ (..Between __ __)'))
const makeFilterPattern = (
  module: Ast.MutableModule,
  columnName: string,
  min: number,
  max: number,
) => {
  return filterPattern.value.instantiateCopied([
    Ast.TextLiteral.new(columnName),
    Ast.tryNumberToEnso(min, module)!,
    Ast.tryNumberToEnso(max, module)!,
  ])
}

const errorMessage = computed(() => data.value.error_message)

function getAstPatternFilterAndSort(
  series: string[],
  xColName: string,
  minX: number,
  maxX: number,
  minY: number,
  maxY: number,
) {
  return Pattern.new((ast) => {
    let pattern: Ast.Owned<Ast.MutableOprApp> | Ast.Owned<Ast.MutableApp> = Ast.App.positional(
      Ast.PropertyAccess.new(ast.module, ast, Ast.identifier('filter')!),
      makeFilterPattern(ast.module, xColName, minX, maxX),
    )
    for (const s of series) {
      pattern = Ast.OprApp.new(
        ast.module,
        pattern,
        '.',
        Ast.App.positional(
          Ast.Ident.new(ast.module, Ast.identifier('filter')!),
          makeFilterPattern(ast.module, s!, minY, maxY),
        ),
      )
    }
    return pattern
  })
}
const createNewFilterNode = () => {
  const seriesLabels = Object.keys(data.value.axis)
    .filter((s) => s != 'x')
    .map((s) => data.value.axis[s as keyof AxesConfiguration].label)
  const xColName = data.value.axis.x.label
  const xItems = data.value.data.map((d) => d.x as number)
  const minX = Math.min(...xItems)
  const maxX = Math.max(...xItems)
  const yItems = data.value.data.map((d) => {
    const { x, label, color, shape, size, row_number, series, ...rest } = d
    return rest
  })
  const yItemsVal = yItems.map((k) => Object.values(k)).flat(1)
  const minY = Math.min(...yItemsVal)
  const maxY = Math.max(...yItemsVal)
  const pattern = getAstPatternFilterAndSort(seriesLabels, xColName, minX, maxX, minY, maxY)
  config.createNodes({
    content: pattern,
    commit: true,
  })
}

function getAstPattern(selector?: number, action?: string) {
  if (action && selector != null) {
    return Pattern.new((ast) =>
      Ast.App.positional(
        Ast.PropertyAccess.new(ast.module, ast, Ast.identifier(action)!),
        Ast.tryNumberToEnso(selector, ast.module)!,
      ),
    )
  }
}

function createNode(rowNumber: number) {
  const selector = data.value.get_row_method
  const pattern = getAstPattern(rowNumber, selector)
  if (pattern) {
    config.createNodes({
      content: pattern,
      commit: true,
    })
  }
}

function formatXPoint(x: Date | number | DateObj) {
  if (data.value.isTimeSeries && x instanceof Date) {
    switch (data.value.x_value_type) {
      case 'Time':
        return x.toTimeString()
      case 'Date':
        return x.toDateString()
      default:
        return x.toString()
    }
  }
  return x
}

function getTooltipMessage(point: Point) {
  if (data.value.is_multi_series) {
    const axis = data.value.axis
    const label =
      point.series && point.series in axis ?
        axis[point.series as keyof AxesConfiguration].label
      : ''
    return `${formatXPoint(point.x)}, ${point.y}, ${label}- Double click to inspect point`
  }
  return `${formatXPoint(point.x)}, ${point.y}- Double click to inspect point`
}

// === Update contents ===

watchPostEffect(() => {
  const xScale_ = data.value.isTimeSeries ? xScaleTime.value : xScale.value
  const yScale_ = yScale.value
  const plotData = getPlotData(data.value) as Point[]
  const series = Object.keys(data.value.axis).filter((s) => s != 'x')
  const colorScale = (d: string) => {
    const color = d3.scaleOrdinal(d3.schemeCategory10).domain(series)
    if (data.value.is_multi_series) {
      return color(d)
    }
    return DEFAULT_FILL_COLOR
  }
  d3Points.value
    .selectAll<SVGPathElement, unknown>('path')
    .data(plotData)
    .join((enter) => enter.append('path'))
    .call((data) => {
      return data.append('title').text((d) => getTooltipMessage(d))
    })
    .on('dblclick', (d) => {
      createNode(d.srcElement.__data__.row_number)
    })
    .transition()
    .duration(animationDuration.value)
    .attr(
      'd',
      symbol.type(matchShape).size((d) => (d.size ?? 0.15) * SIZE_SCALE_MULTIPLER),
    )
    .style('fill', (d) => colorScale(d.series || ''))
    .attr('transform', (d) => `translate(${xScale_(Number(d.x))}, ${yScale_(d.y)})`)
  if (data.value.points.labels === VISIBLE_POINTS) {
    d3Points.value
      .selectAll<SVGPathElement, unknown>('text')
      .data(plotData)
      .join((enter) => enter.append('text').attr('class', 'label'))
      .transition()
      .duration(animationDuration.value)
      .text((d) => d.label ?? '')
      .attr('x', (d) => xScale_(Number(d.x)) + POINT_LABEL_PADDING_X_PX)
      .attr('y', (d) => yScale_(d.y) + POINT_LABEL_PADDING_Y_PX)
  }
})

watchPostEffect(() => {
  if (data.value.is_multi_series) {
    const seriesLabels = Object.keys(data.value.axis)
      .filter((s) => s != 'x')
      .map((s) => {
        return data.value.axis[s as keyof AxesConfiguration].label
      })
    const formatLabel = (string: string) =>
      string.length > 10 ? `${string.substr(0, 10)}...` : string

    const color = d3
      .scaleOrdinal<string>()
      .domain(seriesLabels)
      .range(d3.schemeCategory10)
      .domain(seriesLabels)

    d3Legend.value.selectAll('circle').remove()
    d3Legend.value.selectAll('text').remove()

    d3Legend.value
      .selectAll('dots')
      .data(seriesLabels)
      .enter()
      .append('circle')
      .attr('cx', function (d, i) {
        return 90 + i * 120
      })
      .attr('cy', 10)
      .attr('r', 6)
      .style('fill', (d) => color(d) || DEFAULT_FILL_COLOR)

    d3Legend.value
      .selectAll('labels')
      .data(seriesLabels)
      .enter()
      .append('text')
      .attr('x', function (d, i) {
        return 100 + i * 120
      })
      .attr('y', 10)
      .style('font-size', '15px')
      .text((d) => formatLabel(d))
      .attr('alignment-baseline', 'middle')
      .call((labels) => labels.append('title').text((d) => d))
  }
})

// ======================
// === Event handlers ===
// ======================

function endBrushing() {
  brushExtent.value = undefined
  d3Brush.value.call(brush.value.move, null)
}

useEvent(document, 'click', endBrushing)
useEvent(document, 'auxclick', endBrushing)
useEvent(document, 'contextmenu', endBrushing)
useEvent(document, 'scroll', endBrushing)

/**
 * Zoom into the selected area of the plot.
 *
 * Based on https://www.d3-graph-gallery.com/graph/interactivity_brush.html
 * Section "Brushing for zooming".
 */
function zoomToSelected(override?: boolean) {
  shouldAnimate.value = true
  focus.value = undefined
  const shouldZoomToSelected = override ?? isBrushing.value
  if (!shouldZoomToSelected) {
    shouldAnimate.value = true
    focus.value = undefined
    bounds.value = undefined
    limit.value = DEFAULT_LIMIT
    xDomain.value = [
      Number(extremesAndDeltas.value.xMin) - extremesAndDeltas.value.paddingX,
      Number(extremesAndDeltas.value.xMax) + extremesAndDeltas.value.paddingX,
    ]
    yDomain.value = [
      extremesAndDeltas.value.yMin - extremesAndDeltas.value.paddingY,
      extremesAndDeltas.value.yMax + extremesAndDeltas.value.paddingY,
    ]
  } else {
    if (
      brushExtent.value == null ||
      !Array.isArray(brushExtent.value[0]) ||
      !Array.isArray(brushExtent.value[1])
    )
      return
    const xScale_ = xScale.value
    const yScale_ = yScale.value
    const [[xMinRaw, yMaxRaw], [xMaxRaw, yMinRaw]] = brushExtent.value
    const xMin = xScale_.invert(xMinRaw)
    const xMax = xScale_.invert(xMaxRaw)
    const yMin = yScale_.invert(yMinRaw)
    const yMax = yScale_.invert(yMaxRaw)
    bounds.value = [xMin, yMin, xMax, yMax]
    xDomain.value = [xMin, xMax]
    yDomain.value = [yMin, yMax]
  }
  endBrushing()

  if (!isUsingIndexForX.value && !data.value.isTimeSeries) {
    createNewFilterNodeEnabled.value = true
  }
  if (!override) {
    createNewFilterNodeEnabled.value = false
  }
}

useEvent(document, 'keydown', bindings.handler({ zoomToSelected: () => zoomToSelected() }))

config.setToolbar([
  {
    icon: 'select',
    title: 'Enable Selection',
    toggle: selectionEnabled,
  },
  {
    icon: 'show_all',
    title: 'Fit All',
    onClick: () => zoomToSelected(false),
  },
  {
    icon: 'zoom',
    title: 'Zoom to Selected',
    disabled: () => brushExtent.value == null,
    onClick: zoomToSelected,
  },
  {
    icon: 'add_to_graph_editor',
    title: 'Create component of selected points',
    disabled: () => !createNewFilterNodeEnabled.value,
    onClick: createNewFilterNode,
  },
])
</script>

<template>
  <div v-if="errorMessage" class="WarningsScatterplotVisualization">
    {{ errorMessage }}
  </div>
  <div v-else ref="containerNode" class="ScatterplotVisualization">
    <svg :width="width" :height="height">
      <g ref="legendNode"></g>
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
          v-if="showYLabelText"
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
</template>

<style scoped>
.ScatterplotVisualization {
  user-select: none;
  display: flex;
  flex-direction: column;
}

.WarningsScatterplotVisualization {
  padding: 18px;
}

.ScatterplotVisualization .selection {
  rx: 4px;
  stroke: transparent;
}

.label-y {
  transform: rotate(-90deg);
}
</style>
