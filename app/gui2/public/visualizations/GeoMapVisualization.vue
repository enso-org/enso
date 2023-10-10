<script lang="ts">
export const name = 'Geo Map'
export const inputType = 'Standard.Table.Data.Table.Table'
export const scripts = [
  // mapbox-gl does not have an ESM release.
  'https://api.tiles.mapbox.com/mapbox-gl-js/v2.15.0/mapbox-gl.js',
  // The deck.gl scripting API is not available in the ESM module.
  'https://cdn.jsdelivr.net/npm/deck.gl@8.9.27/dist.min.js',
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

type Data = RegularData | Layer | DataFrame

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
  color?: Color | undefined
  radius?: number | undefined
  label?: string | undefined
}

interface LocationWithPosition {
  position: [longitude: number, latitude: number]
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
declare var deck: typeof import('deck.gl')
</script>

<script setup lang="ts">
/// <reference types="@danmarshall/deckgl-typings" />
import { computed, onMounted, onUnmounted, ref, watchPostEffect } from 'vue'

import type { Deck } from 'deck.gl'

import FindIcon from './icons/find.svg'
import GeoMapDistanceIcon from './icons/geo_map_distance.svg'
import GeoMapPinIcon from './icons/geo_map_pin.svg'
import Path2Icon from './icons/path2.svg'

import { VisualizationContainer } from 'builtins'

const props = defineProps<{ data: Data }>()
const emit = defineEmits<{
  'update:processor': [module: string, method: string]
}>()

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

const dataPoints = ref<LocationWithPosition[]>([])
const mapNode = ref<HTMLElement>()
const latitude = ref(0)
const longitude = ref(0)
const zoom = ref(0)
const mapStyle = ref(DEFAULT_MAP_STYLE)
const pitch = ref(0)
const controller = ref(true)
const showingLabels = ref(true)
const deckgl = ref<Deck>()

const viewState = computed(() => ({
  longitude: longitude.value,
  latitude: latitude.value,
  zoom: zoom.value,
  pitch: pitch.value,
}))

watchPostEffect(() => {
  if (updateState(props.data)) {
    updateMap()
    updateLayers()
  }
})

onMounted(() => {
  dataPoints.value = []
  emit('update:processor', 'Standard.Visualization.Geo_Map', 'process_to_json_text')
})

onUnmounted(() => deckgl.value?.finalize())

/**
 * Update the internal data with the new incoming data. Does not affect anything rendered.
 * Returns true on a successful update and false if no valid data was provided.
 */
function updateState(data: Data) {
  // For now we assume every update has all data. If we move to incremental updates we need
  // to keep the old state and do a proper update.
  resetState()

  extractDataPoints(data)
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
  if (!('df_latitude' in data) && !('data' in data)) {
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

function initDeckGl() {
  if (mapNode.value == null) {
    return
  }
  try {
    deckgl.value = new deck.DeckGL({
      // The `...{}` spread operator suppresses TypeScript's excess property errors.
      // These are valid properties, but they do not exist in the typings.
      ...{
        container: mapNode.value,
        mapboxApiAccessToken: TOKEN,
        mapStyle: mapStyle.value,
      },
      initialViewState: viewState.value,
      controller: controller.value,
    }) as any
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
    console.warn('Geo Map visualization is missing its map container.')
    return
  }
  while (map.lastChild != null) {
    map.removeChild(map.lastChild)
  }
}

function updateDeckGl() {
  const deckgl_ = deckgl.value
  if (deckgl_ == null) {
    console.warn('Geo Map could not update its deck.gl instance.')
    return
  }
  deckgl_.viewState = viewState.value
  // @ts-expect-error
  deckgl_.mapStyle = mapStyle.value
  // @ts-expect-error
  deckgl_.controller = controller.value
}

function updateLayers() {
  if (deckgl.value == null) {
    console.warn(
      'Geo Map visualization could not update its layers ' +
        'due to its deck.gl instance being missing.',
    )
    return
  }
  ;(deckgl.value as any).setProps({
    layers: [
      new deck.ScatterplotLayer<LocationWithPosition>({
        data: dataPoints.value,
        getFillColor: (d) => d.color!,
        getRadius: (d) => d.radius!,
        pickable: showingLabels.value,
      }),
    ],
    getTooltip: ({ object }: { object: { label: string } }) =>
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
          // This is required for it to show above Mapbox's information button.
          zIndex: 2,
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
function extractVisualizationDataFromFullConfig(parsedData: RegularData | Layer) {
  if ('type' in parsedData && parsedData.type === SCATTERPLOT_LAYER && parsedData.data.length) {
    pushPoints(parsedData.data)
  } else if ('layers' in parsedData) {
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
  const newPoints: Location[] = []
  for (let i = 0; i < parsedData.df_latitude.length; i += 1) {
    const latitude = parsedData.df_longitude[i]!
    const longitude = parsedData.df_longitude[i]!
    const label = parsedData.df_label?.[i]
    const color = parsedData.df_color?.[i]
    const radius = parsedData.df_radius?.[i]
    newPoints.push({ latitude, longitude, label, color, radius })
  }
  pushPoints(newPoints)
}

/**
 * Extracts the data form the given `parsedData`. Checks the type of input data and prepares our
 * internal data  (`GeoPoints') for consumption in deck.gl.
 *
 * @param parsedData         - All the parsed data to create points from.
 * @param preparedDataPoints - List holding data points to push the GeoPoints into.
 * @param ACCENT_COLOR        - accent color of IDE if element doesn't specify one.
 */
function extractDataPoints(parsedData: Data) {
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
function pushPoints(newPoints: Location[]) {
  const points = dataPoints.value
  for (const point of newPoints) {
    if (
      typeof point.longitude === 'number' &&
      !Number.isNaN(point.longitude) &&
      typeof point.latitude === 'number' &&
      !Number.isNaN(point.latitude)
    ) {
      let position: [number, number] = [point.longitude, point.latitude]
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
  <VisualizationContainer :overflow="true">
    <template #toolbar>
      <button class="image-button"><img :src="FindIcon" /></button>
      <button class="image-button"><img :src="Path2Icon" /></button>
      <button class="image-button"><img :src="GeoMapDistanceIcon" /></button>
      <button class="image-button"><img :src="GeoMapPinIcon" /></button>
    </template>
    <div ref="mapNode" class="GeoMapVisualization" @wheel.stop></div>
  </VisualizationContainer>
</template>

<style scoped>
@import url('https://api.tiles.mapbox.com/mapbox-gl-js/v2.15.0/mapbox-gl.css');

.GeoMapVisualization {
  height: 100%;
}
</style>

<style>
.GeoMapVisualization > .mapboxgl-map {
  border-radius: 16px;
}
</style>
