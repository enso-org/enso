<script lang="ts">
export const name = 'Heatmap'
export const inputType = 'Standard.Table.Data.Table.Table | Standard.Base.Data.Vector.Vector'
export const defaultPreprocessor = [
  'Standard.Visualization.Table.Visualization',
  'prepare_visualization',
] as const

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

interface Bucket {
  index: number
  group: number
  variable: number
  value: number
}
</script>

<script setup lang="ts">
import { computed, ref, watchPostEffect } from 'vue'

import * as d3 from 'd3'

import VisualizationContainer from '@/components/VisualizationContainer.vue'
import { useVisualizationConfig } from '@/providers/visualizationConfig.ts'

const props = defineProps<{ data: Data }>()

const config = useVisualizationConfig()

const MARGIN = { top: 20, right: 20, bottom: 20, left: 25 }

const data = computed(() => {
  if (props.data == null) {
    console.error('Heatmap was not passed any data.')
    return []
  } else if (props.data.update === 'diff') {
    if (props.data.data != null) {
      return props.data.data
    }
  } else if (props.data.data != null) {
    return props.data.data
  } else if (Array.isArray(props.data)) {
    return props.data
  } else if (props.data.json != null && Array.isArray(props.data.json)) {
    return props.data.json
  }
  return []
})

const containerNode = ref<HTMLElement>()
const pointsNode = ref<SVGElement>()
const xAxisNode = ref<SVGGElement>()
const yAxisNode = ref<SVGGElement>()

const d3XAxis = computed(() => d3.select(xAxisNode.value))
const d3YAxis = computed(() => d3.select(yAxisNode.value))

const d3Points = computed(() => d3.select(pointsNode.value))

const fill = computed(() =>
  d3
    .scaleSequential()
    .interpolator(d3.interpolateViridis)
    .domain([0, d3.max(buckets.value, (d) => d.value) ?? 1]),
)

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
const boxWidth = computed(() => Math.max(0, width.value - MARGIN.left - MARGIN.right))
const boxHeight = computed(() => Math.max(0, height.value - MARGIN.top - MARGIN.bottom))

const buckets = computed(() => {
  const newData = data.value
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
  return Array.from<unknown, Bucket>({ length: groups.length }, (_, i) => ({
    index: i,
    group: groups[i]!,
    variable: variables[i]!,
    value: values[i]!,
  }))
})
const groups = computed(() => Array.from(new Set(Array.from(buckets.value, (p) => p.group))))
const variables = computed(() => Array.from(new Set(Array.from(buckets.value, (p) => p.variable))))

const xScale = computed(() =>
  d3
    .scaleBand<number>()
    .padding(0.05)
    .range([0, boxWidth.value])
    .domain(buckets.value.map((d) => d.group)),
)
const yScale = computed(() =>
  d3
    .scaleBand<number>()
    .padding(0.05)
    .range([boxHeight.value, 0])
    .domain(buckets.value.map((d) => d.variable ?? 0)),
)

const xAxis = computed(() => {
  const xMod = Math.max(1, Math.round(buckets.value.length / (boxWidth.value / 40)))
  const lastGroupIndex = groups.value.length - 1
  return d3
    .axisBottom(xScale.value)
    .tickSize(0)
    .tickValues(groups.value.filter((_, i) => i % xMod === 0 || i === lastGroupIndex))
})

const yAxis = computed(() => {
  const yMod = Math.max(1, Math.round(buckets.value.length / (boxHeight.value / 20)))
  const lastVariableIndex = variables.value.length - 1
  return d3
    .axisLeft(yScale.value)
    .tickSize(0)
    .tickValues(variables.value.filter((_, i) => i % yMod === 0 || i === lastVariableIndex))
})

// ==============
// === Update ===
// ==============

// === Update x axis ===

watchPostEffect(() => d3XAxis.value.call(xAxis.value))

// === Update y axis ===

watchPostEffect(() => d3YAxis.value.call(yAxis.value))

// === Update contents ===

watchPostEffect(() => {
  const buckets_ = buckets.value
  const xScale_ = xScale.value
  const yScale_ = yScale.value
  const fill_ = fill.value
  d3Points.value
    .selectAll('rect')
    .data(buckets_)
    .join((enter) =>
      enter
        .append('rect')
        .attr('rx', 4)
        .attr('ry', 4)
        .style('stroke-width', 4)
        .style('stroke', 'none')
        .style('opacity', 0.8)
        .style('fill', (d) => fill_(d.value)),
    )
    .attr('width', xScale_.bandwidth())
    .attr('height', yScale_.bandwidth())
    .attr('x', (d) => xScale_(d.group)!)
    .attr('y', (d) => yScale_(d.variable ?? 0)!)
})
</script>

<template>
  <VisualizationContainer :belowToolbar="true">
    <div ref="containerNode" class="HeatmapVisualization">
      <svg :width="width" :height="height">
        <g :transform="`translate(${MARGIN.left},${MARGIN.top})`">
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
