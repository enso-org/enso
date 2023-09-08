<script lang="ts">
export const name = 'Geo Map'
export const inputType = 'Standard.Table.Data.Table.Table'

/**
 * Provides a mapbox & deck.gl-based map visualization.
 *
 * > Example creates a map with described properties with a scatter plot overlay:
 * {
 * "latitude": 37.8,
 * "longitude": -122.45,
 * "zoom": 15,
 * "controller": true,
 * "showingLabels": true, // Enables presenting labels when hovering over a point.
 * "layers": [{
 *     "type": "Scatterplot_Layer",
 *     "data": [{
 *         "latitude": 37.8,
 *         "longitude": -122.45,
 *         "color": [255, 0, 0],
 *         "radius": 100,
 *         "label": "an example label"
 *     }]
 * }]
 * }
 *
 * Can also consume a dataframe that has the columns `latitude`, `longitude` and optionally `label`.
 *
 * TODO: Make 2-finger panning behave like in IDE, and RMB zooming. [#1368]
 */

type Data = RegularData | DataFrame

interface RegularData {
  latitude?: number
  longitude?: number
  zoom?: number
  mapStyle?: string
  pitch?: number
  controller?: boolean
  showingLabels?: boolean
  layers: Layer[]
}

interface Layer {
  type: string
  data: Location[]
}

type Color = [red: number, green: number, blue: number]

interface Location {
  latitude: number
  longitude: number
  color?: Color
  radius?: number
  label?: string
}

interface DataFrame {
  df_latitude: number[]
  df_longitude: number[]
  df_color?: Color[]
  df_radius?: number[]
  df_label?: string[]
}

/// <reference types="@danmarshall/deckgl-typings" />
declare var deckGl: typeof import('deck.gl')
</script>

<script setup lang="ts">
import FindIcon from './icons/find.svg'
import Path2Icon from './icons/path2.svg'
import GeoMapDistanceIcon from './icons/geo_map_distance.svg'
import GeoMapPinIcon from './icons/geo_map_pin.svg'

import VisualizationContainer from './VisualizationContainer.vue'

import { computed, onMounted, ref, watchEffect } from 'vue'

// @ts-expect-error
// eslint-disable-next-line no-redeclare
import deckGl from 'https://cdn.jsdelivr.net/npm/deck.gl@8.9.27/+esm'
// import mapboxGl from 'https://cdn.jsdelivr.net/npm/mapbox-gl@2.15.0/+esm'

const props = defineProps<{ data: Data | string }>()
const emit = defineEmits<{
  'update:processor': [module: string, method: string]
}>()

interface Point {
  x: number
  y: number
}

const data = computed<Data>(() =>
  typeof props.data === 'string' ? JSON.parse(props.data) : props.data,
)

/** GeoMap Visualization. */

/**
 * Mapbox API access token.
 * All the limits of API are listed here: https://docs.mapbox.com/api/#rate-limits
 */
const TOKEN =
  'pk.eyJ1IjoiZW5zby1vcmciLCJhIjoiY2tmNnh5MXh2MGlyOTJ5cWdubnFxbXo4ZSJ9.3KdAcCiiXJcSM18nwk09-Q'
const SCATTERPLOT_LAYER = 'Scatterplot_Layer'
const DEFAULT_POINT_RADIUS = 150

const LABEL_FONT = 'DejaVuSansMonoBook, sans-serif'
const LABEL_FONT_SIZE = '12px'
const LABEL_BORDER_RADIUS = '14px'
const LABEL_BORDER_TOP_LEFT_RADIUS = '2px'
const LABEL_MARGIN = '4px'
const LABEL_BACKGROUND_COLOR = `rgb(252, 250, 245)`
const LABEL_OUTLINE = `rgb(200, 210, 210)`
const LABEL_COLOR = `rgba(0, 0, 0, 0.8)`
const DEFAULT_MAP_STYLE = 'mapbox://styles/mapbox/light-v9'

const DEFAULT_MAP_ZOOM = 11
const ACCENT_COLOR: Color = [78, 165, 253]

const dataPoints = ref<Location[]>([])
const mapNode = ref<HTMLElement>()
const latitude = ref(0)
const longitude = ref(0)
const zoom = ref(0)
const mapStyle = ref(DEFAULT_MAP_STYLE)
const pitch = ref(0)
const controller = ref(true)
const showingLabels = ref(true)
const deckgl = ref<import('deck.gl').DeckGL | null>()

watchEffect(() => {
  if (updateState(data.value)) {
    updateMap()
    updateLayers()
  }
})

onMounted(() => {
  dataPoints.value = []
  emit('update:processor', 'Standard.Visualization.Geo_Map', 'process_to_json_text')
})

/**
 * Update the internal data with the new incoming data. Does not affect anything rendered.
 * Returns true on a successful update and false if no valid data was provided.
 */
function updateState(data: Data) {
  // For now we assume every update has all data. If we move to incremental updates we need
  // to keep the old state and do a proper update.
  resetState()

  extractDataPoints(data, dataPoints.value)
  // eslint-disable-next-line no-self-assign
  dataPoints.value = dataPoints.value
  if (dataPoints.value.length === 0) {
    // We have no valid data and should skip initialization.
    return false
  }
  const center = centerPoint()

  latitude.value = center.latitude
  longitude.value = center.longitude
  zoom.value = DEFAULT_MAP_ZOOM
  mapStyle.value = DEFAULT_MAP_STYLE
  pitch.value = 0
  controller.value = true
  showingLabels.value = true
  if (!('df_latitude' in data)) {
    latitude.value = data.latitude ?? center.latitude
    longitude.value = data.longitude ?? center.longitude
    // TODO: Compute zoom somehow from span of latitudes and longitudes.
    zoom.value = data.zoom ?? DEFAULT_MAP_ZOOM
    mapStyle.value = data.mapStyle ?? DEFAULT_MAP_STYLE
    pitch.value = data.pitch ?? 0
    controller.value = data.controller ?? true
    showingLabels.value = data.showingLabels ?? false
  }
  return true
}

function updateMap() {
  if (deckgl.value == null) {
    initDeckGl()
  } else {
    updateDeckGl()
  }
}

function makeScatterLayer() {
  return new deckGl.ScatterplotLayer<Location>({
    data: dataPoints.value,
    getFillColor: (d) => d.color!,
    getRadius: (d) => d.radius!,
    pickable: showingLabels.value,
  })
}

function initDeckGl() {
  try {
    deckgl.value = new deckGl.DeckGL({
      container: mapId,
      mapboxApiAccessToken: TOKEN,
      mapStyle: mapStyle.value,
      initialViewState: viewState(),
      controller: controller.value,
    })
  } catch (error) {
    console.error(error)
    resetState()
    resetDeckGl()
  }
}

/**
 * Reset the internal state of the visualization, discarding all previous data updates.
 */
function resetState() {
  // We only need to reset the data points as everything else will be overwritten when new data
  // arrives.
  dataPoints.value = []
}

function resetDeckGl() {
  deckgl.value = undefined
  resetMapElement()
}

function resetMapElement() {
  const map = mapNode.value
  if (map == null) {
    return
  }
  while (map.lastChild != null) {
    map.removeChild(map.lastChild)
  }
}

function updateDeckGl() {
  if (deckgl.value != null) {
    deckgl.value.viewState = viewState()
    deckgl.value.mapStyle = mapStyle.value
    deckgl.value.controller = controller.value
  }
}

function updateLayers() {
  if (deckgl.value == null) {
    return
  }
  deckgl.value.setProps({
    layers: [makeScatterLayer()],
    getTooltip: ({ object }) =>
      object && {
        html: `<div>${object.label}</div>`,
        style: {
          backgroundColor: LABEL_BACKGROUND_COLOR,
          fontSize: LABEL_FONT_SIZE,
          borderRadius: LABEL_BORDER_RADIUS,
          borderTopLeftRadius: LABEL_BORDER_TOP_LEFT_RADIUS,
          fontFamily: LABEL_FONT,
          margin: LABEL_MARGIN,
          color: LABEL_COLOR,
          border: '1px solid ' + LABEL_OUTLINE,
        },
      },
  })
}

/**
 * Calculate the center of the bounding box of the given list of objects. The objects need to have
 * a `position` attribute with two coordinates.
 */
function centerPoint() {
  let minX = 0
  let maxX = 0
  let minY = 0
  let maxY = 0
  {
    const xs = dataPoints.value.map((p) => p.position[0])
    minX = Math.min(...xs)
    maxX = Math.min(...xs)
  }
  {
    const ys = dataPoints.value.map((p) => p.position[1])
    minY = Math.min(...ys)
    maxY = Math.min(...ys)
  }
  let longitude = (minX + maxX) / 2
  let latitude = (minY + maxY) / 2
  return { latitude, longitude }
}

/**
 * Extract the visualization data from a full configuration object.
 */
function extractVisualizationDataFromFullConfig(parsedData: Location[]) {
  if (parsedData.type === SCATTERPLOT_LAYER && parsedData.data.length) {
    pushPoints(parsedData.data)
  } else if (parsedData.layers != null) {
    parsedData.layers.forEach((layer) => {
      if (layer.type === SCATTERPLOT_LAYER) {
        let dataPoints = layer.data ?? []
        pushPoints(dataPoints)
      } else {
        console.warn('Geo_Map: Currently unsupported deck.gl layer.')
      }
    })
  }
  // eslint-disable-next-line no-self-assign
  dataPoints.value = dataPoints.value
}

/**
 * Extract the visualization data from a dataframe.
 */
function extractVisualizationDataFromDataFrame(parsedData: DataFrame) {
  const points = dataPoints.value
  for (let i = 0; i < parsedData.df_latitude.length; i += 1) {
    const latitude = parsedData.df_longitude[i]
    const longitude = parsedData.df_longitude[i]
    const label = parsedData.df_label?.[i]
    const color = parsedData.df_color?.[i]
    const radius = parsedData.df_radius?.[i]
    points.push({ latitude, longitude, label, color, radius })
  }
  // eslint-disable-next-line no-self-assign
  dataPoints.value = dataPoints.value
}

/**
 * Extracts the data form the given `parsedData`. Checks the type of input data and prepares our
 * internal data  (`GeoPoints') for consumption in deck.gl.
 *
 * @param parsedData         - All the parsed data to create points from.
 * @param preparedDataPoints - List holding data points to push the GeoPoints into.
 * @param ACCENT_COLOR        - accent color of IDE if element doesn't specify one.
 */
function extractDataPoints(parsedData: Location[] | DataFrame) {
  if ('df_latitude' in parsedData && 'df_longitude' in parsedData) {
    extractVisualizationDataFromDataFrame(parsedData)
  } else {
    extractVisualizationDataFromFullConfig(parsedData)
  }
}

/**
 * Transforms the `dataPoints` to the internal data format and appends them to the `targetList`.
 * Also adds the `ACCENT_COLOR` for each point.
 *
 * Expects the `dataPoints` to be a list of objects that have a `longitude` and `latitude` and
 * optionally `radius`, `color` and `label`.
 */
function pushPoints(newPoints) {
  const points = dataPoints.value
  for (const point of newPoints) {
    if (
      typeof point.longitude === 'number' &&
      !Number.isNaN(point.longitude) &&
      typeof point.latitude === 'number' &&
      !Number.isNaN(point.latitude)
    ) {
      let position = [point.longitude, point.latitude]
      let radius =
        typeof point.radius === 'number' && !Number.isNaN(point.radius)
          ? point.radius
          : DEFAULT_POINT_RADIUS
      let color = point.color ?? ACCENT_COLOR
      let label = point.label ?? ''
      points.push({ position, color, radius, label })
    }
  }
}
</script>

<template>
  <VisualizationContainer :="<any>$attrs">
    <template #toolbar>
      <button class="image-button"><img :src="FindIcon" /></button>
      <button class="image-button"><img :src="Path2Icon" /></button>
      <button class="image-button"><img :src="GeoMapDistanceIcon" /></button>
      <button class="image-button"><img :src="GeoMapPinIcon" /></button>
    </template>
    <div ref="mapNode"></div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://api.mapbox.com/mapbox-gl-js/v2.1.1/mapbox-gl.css');
</style>

<style>
.mapboxgl-map {
  border-radius: 14px;
}
</style>
