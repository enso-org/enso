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
</script>

<script setup lang="ts">
/** Heatmap Visualization. */
// TODO refactor this to avoid loading on startup. See issue #985 .

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7.8.5/+esm'

import VisualizationContainer from './VisualizationContainer.vue'

import { computed, onMounted, ref, watch } from 'vue'

const props = defineProps<{
  width: number | undefined
  height: number | undefined
  data: Data | string
}>()
const emit = defineEmits<{
  'update:preprocessor': [module: string, method: string, ...args: string[]]
}>()

onMounted(() => {
  emit('update:preprocessor', 'Standard.Visualization.Table.Visualization', 'prepare_visualization')
})

const points = ref<object[]>([])

const containerNode = ref<HTMLElement>()

const width = computed(
  () => props.width ?? containerNode.value?.getBoundingClientRect().width ?? 100,
)
const height = computed(
  () => props.height ?? ((containerNode.value?.getBoundingClientRect().width ?? 100) * 3) / 4,
)
const canvasWidth = computed(() => Math.max(0, width.value - margin.left - margin.right))
const canvasHeight = computed(() => Math.max(0, height.value - margin.top - margin.bottom))

watch(
  () => props.data,
  () => {
    const newData: Data = typeof props.data === 'string' ? JSON.parse(props.data) : props.data
    if (newData.update === 'diff') {
      if (newData.data != null) {
        points.value = newData.data
      }
    } else if (newData.data != null) {
      points.value = newData.data
    } else if (Array.isArray(newData)) {
      points.value = newData
    } else if (newData.json != null && Array.isArray(newData.json)) {
      points.value = newData.json
    }
  },
)

const margin = { top: 20, right: 20, bottom: 20, left: 25 }

/**
 * Creates HTML div element as container for plot.
 */
function createOuterContainerWithStyle(width, height) {
  const divElem = document.createElementNS(null, 'div')
  divElem.setAttributeNS(null, 'class', 'vis-heatmap')
  divElem.setAttributeNS(null, 'viewBox', 0 + ' ' + 0 + ' ' + width + ' ' + height)
  divElem.setAttributeNS(null, 'width', '100%')
  divElem.setAttributeNS(null, 'height', '100%')

  const addStyleToElem = (attr, stl) => {
    const style = document.createElement('style')
    style.innerText = attr + '{' + stl + '}'

    divElem.appendChild(style)
  }

  const darkStrokeColor = `rgba(255,255,255,0.7)`

  addStyleToElem('.dark-theme text', `fill: ${darkStrokeColor};`)

  return divElem
}

/**
 * Initialise the drawing svg and related properties, e.g., canvas size and margins.
 */
function initCanvas() {
  while (this.dom.firstChild) {
    this.dom.removeChild(this.dom.lastChild)
  }

  this.canvas = this.canvasDimensions()
  const container = this.createOuterContainerWithStyle(
    this.canvas.outer.width,
    this.canvas.outer.height,
  )
  this.dom.appendChild(container)

  this.svg = d3
    .select(container)
    .append('svg')
    .attr('width', this.canvas.outer.width)
    .attr('height', this.canvas.outer.height)
    .append('g')
    .attr('transform', 'translate(' + this.canvas.margin.left + ',' + this.canvas.margin.top + ')')
}

/**
 * Initialise the heatmap with the current data and settings.
 */
function initHeatmap() {
  let data = this.data()
  if (ok(data.length) && data.length === 2 && Array.isArray(data[1])) {
    let indices = Array.from(Array(data[1].length).keys())
    data = [data[0], indices, data[1]]
  } else if (!Array.isArray(data[0])) {
    let indices = Array.from(Array(data.length).keys())
    data = [indices, [], data]
  } else if (ok(data.length) && data.length === 1 && Array.isArray(data[0])) {
    let indices = Array.from(Array(data[0].length).keys())
    data = [indices, [], data[0]]
  }

  // Labels of row and columns
  let myGroups = d3.map(data[0], (d) => d).keys()
  let myVars = d3.map(data[1], (d) => d).keys()
  let labelStyle = 'font-family: DejaVuSansMonoBook; font-size: 10px;'
  let self = this

  // Build X scales and axis:
  let x = d3.scaleBand().range([0, this.canvas.inner.width]).domain(myGroups).padding(0.05)
  let xAxis = d3
    .axisBottom(x)
    .tickSize(0)
    .tickValues(
      myGroups.filter((d, i) => {
        if (i === myGroups.length - 1) {
          return 1
        }
        let divisor = (5 * self.canvas.outer.width) / 200
        return !(i % Math.round(myGroups.length / divisor))
      }),
    )

  this.svg
    .append('g')
    .attr('style', labelStyle)
    .attr('transform', 'translate(0,' + this.canvas.inner.height + ')')
    .call(xAxis)
    .select('.domain')
    .remove()

  // Build Y scales and axis:
  let y = d3.scaleBand().range([this.canvas.inner.height, 0]).domain(myVars).padding(0.05)
  let yAxis = d3
    .axisLeft(y)
    .tickSize(0)
    .tickValues(
      myVars.filter((d, i) => {
        if (i === myVars.length - 1) {
          return 1
        }
        let divisor = (9 * self.canvas.outer.height) / 200
        return !(i % Math.round(myVars.length / divisor))
      }),
    )
  this.svg.append('g').attr('style', labelStyle).call(yAxis).select('.domain').remove()

  // Build color scale
  let fill = d3
    .scaleSequential()
    .interpolator(d3.interpolateViridis)
    .domain([0, d3.max(data[2], (d) => d)])

  let indices = Array.from(Array(data[0].length).keys())

  this.svg
    .selectAll()
    .data(indices, (d) => data[0][d] + ':' + data[1][d])
    .enter()
    .append('rect')
    .attr('x', (d) => x(data[0][d]))
    .attr('y', (d) => y(data[1][d]))
    .attr('rx', 4)
    .attr('ry', 4)
    .attr('width', x.bandwidth())
    .attr('height', y.bandwidth())
    .style('fill', (d) => fill(data[2][d]))
    .style('stroke-width', 4)
    .style('stroke', 'none')
    .style('opacity', 0.8)
}

/**
 * Sets size of the main parent DOM object.
 */
function setSize(size) {
  while (this.dom.firstChild) {
    this.dom.removeChild(this.dom.lastChild)
  }

  this.dom.setAttributeNS(null, 'width', size[0])
  this.dom.setAttributeNS(null, 'height', size[1])
  this.initCanvas()
  this.initHeatmap()
}
</script>

<template>
  <VisualizationContainer
    :="<any>$attrs"
    :below-toolbar="true"
    :width="props.width"
    :height="props.height"
  >
    <div ref="containerNode" class="HeatmapVisualization">
      <svg :width="canvasWidth" :height="canvasHeight">
        <g :transform="`translate(${margin.left},${margin.top})`"></g>
      </svg>
    </div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://fonts.cdnfonts.com/css/dejavu-sans-mono');
</style>
