<script lang="ts">
export const name = 'Geo Map'
export const inputType = 'Standard.Table.Data.Table.Table'
export const scripts = [
  'https://unpkg.com/deck.gl@8.4/dist.min.js',
  'https://api.mapbox.com/mapbox-gl-js/v2.1.1/mapbox-gl.js',
]

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
</script>

<script setup lang="ts">
import FindIcon from './icons/find.svg'
import Path2Icon from './icons/path2.svg'
import GeoMapDistanceIcon from './icons/geo_map_distance.svg'
import GeoMapPinIcon from './icons/geo_map_pin.svg'

import VisualizationContainer from './VisualizationContainer.vue'

import { computed, onMounted, ref, watchEffect } from 'vue'

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
  if (deckgl == null) {
    initDeckGl()
  } else {
    updateDeckGl()
  }
}

function makeScatterLayer() {
  return new deck.ScatterplotLayer({
    data: dataPoints,
    getFillColor: (d) => d.color,
    getRadius: (d) => d.radius,
    pickable: showingLabels,
  })
}

function initDeckGl() {
  try {
    deckgl = new deck.DeckGL({
      container: mapId,
      mapboxApiAccessToken: TOKEN,
      mapStyle: mapStyle,
      initialViewState: viewState(),
      controller: controller,
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
  // We only need to reset the data points as everything else will be overwritten when new
  // data arrives.
  dataPoints = []
}

function resetDeckGl() {
  deckgl = undefined
  resetMapElement()
}

function resetMapElement() {
  while (mapElem.hasChildNodes()) {
    mapElem.removeChild(mapElem.childNodes[0])
  }
}

function updateDeckGl() {
  deckgl.viewState = viewState()
  deckgl.mapStyle = mapStyle
  deckgl.controller = controller
}

function updateLayers() {
  if (deckgl == null) {
    return
  }
  deckgl.setProps({
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

function centerPoint() {
  const { x, y } = calculateCenterPoint(dataPoints)
  return { latitude: y, longitude: x }
}

/**
 * Extract the visualization data from a full configuration object.
 */
function extractVisualizationDataFromFullConfig(
  parsedData: Location[],
  preparedDataPoints: Location[],
) {
  if (parsedData.type === SCATTERPLOT_LAYER && parsedData.data.length) {
    pushPoints(parsedData.data, preparedDataPoints)
  } else if (parsedData.layers != null) {
    parsedData.layers.forEach((layer) => {
      if (layer.type === SCATTERPLOT_LAYER) {
        let dataPoints = layer.data ?? []
        pushPoints(dataPoints, preparedDataPoints)
      } else {
        console.warn('Geo_Map: Currently unsupported deck.gl layer.')
      }
    })
  }
}

/**
 * Extract the visualization data from a dataframe.
 */
function extractVisualizationDataFromDataFrame(
  parsedData: LocationDataFrame,
  preparedDataPoints: Location[],
) {
  for (let i = 0; i < parsedData.df_latitude.length; i += 1) {
    const latitude = parsedData.df_longitude[i]
    const longitude = parsedData.df_longitude[i]
    const label = parsedData.df_label?.[i]
    const color = parsedData.df_color?.[i]
    const radius = parsedData.df_radius?.[i]
    return preparedDataPoints.push({ latitude, longitude, label, color, radius })
  }
}

/**
 * Extracts the data form the given `parsedData`. Checks the type of input data and prepares our
 * internal data  (`GeoPoints') for consumption in deck.gl.
 *
 * @param parsedData         - All the parsed data to create points from.
 * @param preparedDataPoints - List holding data points to push the GeoPoints into.
 * @param ACCENT_COLOR        - accent color of IDE if element doesn't specify one.
 */
function extractDataPoints(
  parsedData: Location[] | LocationDataFrame,
  preparedDataPoints: Location[],
) {
  if ('df_latitude' in parsedData && 'df_longitude' in parsedData) {
    extractVisualizationDataFromDataFrame(parsedData, preparedDataPoints)
  } else {
    extractVisualizationDataFromFullConfig(parsedData, preparedDataPoints)
  }
}

/**
 * Transforms the `dataPoints` to the internal data format and appends them to the `targetList`.
 * Also adds the `ACCENT_COLOR` for each point.
 *
 * Expects the `dataPoints` to be a list of objects that have a `longitude` and `latitude` and
 * optionally `radius`, `color` and `label`.
 */
function pushPoints(dataPoints, targetList) {
  dataPoints.forEach((geoPoint) => {
    if (
      typeof geoPoint.longitude === 'number' &&
      !Number.isNaN(geoPoint.longitude) &&
      typeof geoPoint.latitude === 'number' &&
      !Number.isNaN(geoPoint.latitude)
    ) {
      let position = [geoPoint.longitude, geoPoint.latitude]
      let radius = isValidNumber(geoPoint.radius) ? geoPoint.radius : DEFAULT_POINT_RADIUS
      let color = geoPoint.color ?? ACCENT_COLOR
      let label = geoPoint.label ?? ''
      targetList.push({ position, color, radius, label })
    }
  })
}

/**
 * Calculate the bounding box of the given list of objects. The objects need to have
 * a `position` attribute with two coordinates.
 */
function calculateExtent(dataPoints) {
  const xs = []
  const ys = []
  dataPoints.forEach((e) => {
    xs.push(e.position[0])
    ys.push(e.position[1])
  })
  if (xs.length && ys.length) {
    let minX = Math.min(...xs)
    let maxX = Math.max(...xs)
    let minY = Math.min(...ys)
    let maxY = Math.max(...ys)
    return { minX, maxX, minY, maxY }
  }
  return undefined
}

/**
 * Calculate the center of the bounding box of the given list of objects. The objects need to have
 * a `position` attribute with two coordinates.
 */
function calculateCenterPoint(dataPoints): Point {
  let { minX, maxX, minY, maxY } = calculateExtent(dataPoints)
  let x = (minX + maxX) / 2
  let y = (minY + maxY) / 2
  return { x, y }
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
