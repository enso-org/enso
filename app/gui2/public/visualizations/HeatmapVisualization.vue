<script lang="ts">
export const name = 'Heatmap'
export const inputType = 'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector'

type Data = HeatmapData | HeatmapArrayData | HeatmapJSONData | HeatmapUpdate

interface HeatmapData {
  update: undefined
  data: object[]
  json: undefined
}

type HeatmapArrayData = object[] & {
  update: undefined
  data: undefined
  json: undefined
}

interface HeatmapJSONData {
  update: undefined
  data: undefined
  json: object[]
}

interface HeatmapUpdate {
  update: 'diff'
  data: object[] | undefined
}

interface DataPoint {
  index: number
  group: number
  variable: number
  value: number
}

// eslint-disable-next-line no-redeclare
declare var d3: typeof import('d3')
</script>

<script setup lang="ts">
/** Heatmap Visualization. */
// TODO refactor this to avoid loading on startup. See issue #985 .

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7.8.5/+esm'

import VisualizationContainer from 'builtins/VisualizationContainer.vue'
import { useVisualizationConfig } from 'builtins/useVisualizationConfig.ts'

import { computed, onMounted, ref, watch } from 'vue'

const props = defineProps<{ data: Data | string }>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

const config = useVisualizationConfig()

const containerNode = ref<HTMLElement>()
const pointsNode = ref<SVGElement>()
const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()

const data_ = computed(() => {
  const newData: Data = typeof props.data === 'string' ? JSON.parse(props.data) : props.data
  if (newData.update === 'diff') {
    if (newData.data != null) {
      return newData.data
    }
  } else if (newData.data != null) {
    return newData.data
  } else if (Array.isArray(newData)) {
    return newData
  } else if (newData.json != null && Array.isArray(newData.json)) {
    return newData.json
  }
  return []
})

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Table.Visualization', 'prepare_visualization')
  updateHeatmap()
})

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
const boxWidth = computed(() => Math.max(0, width.value - margin.left - margin.right))
const boxHeight = computed(() => Math.max(0, height.value - margin.top - margin.bottom))

watch(
  () => [data_.value, width.value, height.value],
  () => {
    updateHeatmap()
  },
)

const margin = { top: 20, right: 20, bottom: 20, left: 25 }

const dataPoints = computed(() => {
  const newData = data_.value
  let groups: number[] = []
  let variables: number[] = []
  let values: number[] = []
  if (newData.length != null && newData.length === 2 && Array.isArray(newData[1])) {
    const indices = Array.from(Array(newData[1].length).keys())
    groups = newData[0] as any
    variables = indices
    values = newData[1]
  } else if (!Array.isArray(newData[0])) {
    const indices = Array.from(Array(newData.length).keys())
    groups = indices
    variables = []
    values = newData as any
  } else if (newData.length != null && newData.length === 1 && Array.isArray(newData[0])) {
    const indices = Array.from(Array(newData[0].length).keys())
    groups = indices
    variables = []
    values = newData[0]
  } else {
    groups = newData[0] as any
    variables = newData[1] as any
    values = newData[2] as any
  }

  return Array.from<unknown, DataPoint>({ length: groups.length }, (_, i) => ({
    index: i,
    group: groups[i]!,
    variable: variables[i]!,
    value: values[i]!,
  }))
})

const d3Points = computed(() => {
  if (xAxisNode.value == null || yAxisNode.value == null || pointsNode.value == null) {
    // Not mounted yet.
    return
  }
  const dataPoints_ = dataPoints.value

  // Build color scale
  let fill = d3
    .scaleSequential()
    .interpolator(d3.interpolateViridis)
    .domain([0, d3.max(dataPoints_, (d) => d.value) ?? 1])

  return d3
    .select<SVGElement, DataPoint>(pointsNode.value)
    .selectAll()
    .data(dataPoints.value, (d) => d?.group + ':' + d?.variable)
    .enter()
    .append('rect')
    .attr('rx', 4)
    .attr('ry', 4)
    .style('fill', (d) => fill(d.value))
    .style('stroke-width', 4)
    .style('stroke', 'none')
    .style('opacity', 0.8)
})

const groups = computed(() => Array.from(new Set(Array.from(dataPoints.value, (p) => p.group))))
const variables = computed(() =>
  Array.from(new Set(Array.from(dataPoints.value, (p) => p.variable))),
)

/**
 * Initialise the heatmap with the current data and settings.
 */
function updateHeatmap() {
  if (xAxisNode.value == null) {
    throw new Error('Could not find the HTML element for the x axis.')
  }
  if (yAxisNode.value == null) {
    throw new Error('Could not find the HTML element for the y axis.')
  }
  if (pointsNode.value == null) {
    throw new Error('Could not find the HTML element for the heatmap.')
  }
  const dataPoints_ = dataPoints.value

  // Build X scales and axis:
  let x = d3
    .scaleBand<number>()
    .range([0, boxWidth.value])
    .domain(dataPoints_.map((d) => d.group))
    .padding(0.05)
  const xMod = Math.max(1, Math.round(dataPoints_.length / (boxWidth.value / 40)))
  const lastGroupIndex = groups.value.length - 1
  let xAxis = d3
    .axisBottom(x)
    .tickSize(0)
    .tickValues(groups.value.filter((_, i) => i % xMod === 0 || i === lastGroupIndex))
  d3.select(xAxisNode.value).call(xAxis)

  // Build Y scales and axis:
  let y = d3
    .scaleBand<number>()
    .range([boxHeight.value, 0])
    .domain(dataPoints_.map((d) => d.variable ?? 0))
    .padding(0.05)
  const yMod = Math.max(1, Math.round(dataPoints_.length / (boxHeight.value / 40)))
  const lastVariableIndex = variables.value.length - 1
  let yAxis = d3
    .axisLeft(y)
    .tickSize(0)
    .tickValues(variables.value.filter((_, i) => i % yMod === 0 || i === lastVariableIndex))
  d3.select(yAxisNode.value).call(yAxis)

  d3Points.value
    ?.attr('x', (d) => x(d.group)!)
    .attr('y', (d) => y(d.variable ?? 0)!)
    .attr('width', x.bandwidth())
    .attr('height', y.bandwidth())
}
</script>

<template>
  <VisualizationContainer :below-toolbar="true">
    <div ref="containerNode" class="HeatmapVisualization">
      <svg :width="width" :height="height">
        <g :transform="`translate(${margin.left},${margin.top})`">
          <g ref="xAxisNode" class="label label-x" :transform="`translate(0, ${boxHeight})`"></g>
          <g ref="yAxisNode" class="label label-y"></g>
          <g ref="pointsNode"></g>
        </g>
      </svg>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://fonts.cdnfonts.com/css/dejavu-sans-mono');

.HeatmapVisualization {
  display: flex;
}

.label {
  font-family: 'DejaVu Sans Mono', monospace;
  font-size: 10px;
}
</style>

<style>
.HeatmapVisualization .label .domain {
  display: none;
}
</style>
