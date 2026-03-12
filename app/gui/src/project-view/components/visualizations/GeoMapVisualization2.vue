<script lang="ts">
export const name = 'Geo Map 2'
export const icon = 'compass'
export const inputType = 'Standard.Table.Table.Table'
export const defaultPreprocessor = [
  'Standard.Visualization.Geo_Map',
  'process_to_json_text',
] as const
export const scripts = [
  // mapbox-gl does not have an ESM release.
  'https://api.tiles.mapbox.com/mapbox-gl-js/v3.19.1/mapbox-gl.js',
]
export const styles = ['https://api.tiles.mapbox.com/mapbox-gl-js/v3.19.1/mapbox-gl.css']

const DEFAULT_COLOR = 'rgb(78, 165, 253)'
const DEFAULT_RADIUS = 8
const DEFAULT_MAP_ZOOM = 11
const DEFAULT_MAX_MAP_ZOOM = 18
const FIT_PADDING = 10
const DATA_LAYER_PREFIX = 'data-layer-'

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

interface ScatterplotLayer {
  type: 'Scatterplot_Layer'
  data: Location[]
}

interface GeoJsonLayer {
  type: 'GeoJsonLayer'
  data: GeoJSON.GeoJSON
}

type Layer = ScatterplotLayer | GeoJsonLayer

type Color = [red: number, green: number, blue: number]

interface Location {
  latitude: number
  longitude: number
  color?: Color | undefined
  radius?: number | undefined
  label?: string | undefined
}

interface DataFrame {
  df_latitude: number[]
  df_longitude: number[]
  df_color?: Color[]
  df_radius?: number[]
  df_label?: string[]
}

declare const mapboxgl: typeof import('mapbox-gl')
</script>

<script setup lang="ts">
import { useMapboxToken } from '$/providers/mapboxToken'
import { useVisualizationConfig } from '@/util/visualizationBuiltins'
// import mapboxgl from 'mapbox-gl'
import bbox from '@turf/bbox'
import {
  computed,
  effectScope,
  onMounted,
  onUnmounted,
  useTemplateRef,
  watch,
  watchEffect,
} from 'vue'

const props = defineProps<{ data: Data }>()
const config = useVisualizationConfig()

const mapboxTokenStore = useMapboxToken()
/**
 * Mapbox API access token.
 * All the limits of API are listed here: https://docs.mapbox.com/api/#rate-limits
 */
const token = await mapboxTokenStore.acquire()

watchEffect(() => ((mapboxgl as any).accessToken = token.value.token))

const mapNode = useTemplateRef('mapNode')
let map: mapboxgl.Map | undefined
const mapLayers: string[] = []

function layerToGeoJSON(layer: Layer): GeoJSON.GeoJSON {
  switch (layer.type) {
    case 'Scatterplot_Layer':
      return {
        type: 'FeatureCollection',
        features: layer.data.map((location) => ({
          type: 'Feature',
          geometry: {
            type: 'Point',
            coordinates: [location.longitude, location.latitude],
          },
          properties: {
            color: location.color,
            radius: location.radius,
            label: location.label,
          },
        })),
      }
    case 'GeoJsonLayer':
      return layer.data
  }
}

function dataframeToGeoJSON(df: DataFrame): GeoJSON.GeoJSON {
  const geojson: GeoJSON.GeoJSON = {
    type: 'FeatureCollection',
    features: [],
  }
  for (let i = 0; i < df.df_latitude.length; i += 1) {
    const latitude = df.df_latitude[i]!
    const longitude = df.df_longitude[i]!
    const label = df.df_label?.[i]
    const color = df.df_color?.[i]
    const radius = df.df_radius?.[i]
    geojson.features.push({
      type: 'Feature',
      geometry: { type: 'Point', coordinates: [longitude, latitude] },
      properties: { label, color, radius },
    })
  }
  return geojson
}

const dataAsGeoJSONs = computed(() => {
  if ('type' in props.data) {
    return [layerToGeoJSON(props.data)]
  } else if ('df_latitude' in props.data) {
    return [dataframeToGeoJSON(props.data)]
  } else {
    return props.data.layers.map(layerToGeoJSON)
  }
})

function updateMap(map: mapboxgl.Map) {
  console.debug(dataAsGeoJSONs.value)
  for (const oldLayer of mapLayers) {
    map.removeLayer(oldLayer)
    map.removeSource(oldLayer)
  }
  mapLayers.length = 0
  let finalBBox: mapboxgl.LngLatBounds | undefined
  dataAsGeoJSONs.value.forEach((geojson, index) => {
    const layerId = `${DATA_LAYER_PREFIX}${index}`
    map.addLayer({
      id: layerId,
      type: 'circle',
      source: {
        type: 'geojson',
        data: geojson,
      },
      paint: {
        'circle-radius': ['coalesce', ['get', 'radius'], DEFAULT_RADIUS],
        'circle-color': ['coalesce', ['get', 'color'], DEFAULT_COLOR],
      },
    })
    mapLayers.push(layerId)
    const layerBbox = bbox(geojson)
    const layerBboxFlat: [number, number, number, number] =
      layerBbox.length == 4 ? layerBbox : [layerBbox[0], layerBbox[1], layerBbox[3], layerBbox[4]]
    finalBBox = finalBBox?.extend(layerBboxFlat) ?? new mapboxgl.LngLatBounds(layerBboxFlat)
  })
  if (finalBBox != null) {
    map.fitBounds(finalBBox, { padding: FIT_PADDING, maxZoom: DEFAULT_MAX_MAP_ZOOM, duration: 500 })
  }
}

function setupTooltip(map: mapboxgl.Map) {
  const popup = new mapboxgl.Popup({
    anchor: 'top-left',
    closeButton: false,
    closeOnClick: false,
    className: 'tooltip',
    offset: 4,
  })

  map.on('mousemove', (event) => {
    const feature = map.queryRenderedFeatures(event.point)[0]
    if (feature?.properties?.label) {
      popup.setLngLat(event.lngLat).setText(feature.properties.label).addTo(map)
    } else {
      popup.remove()
    }
  })
  map.on('mouseout', () => popup.remove())
}

const scope = effectScope()
onMounted(() => {
  if (mapNode.value == null) {
    console.error('Cannot initialize MapBoxGL: no container element!')
    return
  }
  const newMap = new mapboxgl.Map({
    container: mapNode.value,
    projection: 'mercator',
    zoom: DEFAULT_MAP_ZOOM,
  })
  // Hotfix for https://github.com/mapbox/mapbox-gl-js/issues/13355
  ;(newMap as any)._updateContainerDimensions = function () {
    if (!this._container) return

    const width = this._container.offsetWidth || 400
    const height = this._container.offsetHeight || 300

    this._containerWidth = width
    this._containerHeight = height
  }
  newMap.on('load', () => {
    updateMap(newMap)
    scope.run(() => watch(dataAsGeoJSONs, () => updateMap(newMap)))
  })
  setupTooltip(newMap)
  map = newMap
})
onUnmounted(() => map?.remove())
config.setToolbarOverlay(true)
</script>

<template>
  <link
    href="https://api.tiles.mapbox.com/mapbox-gl-js/v3.19.1/mapbox-gl.css"
    rel="stylesheet"
    crossorigin="anonymous"
  />
  <div ref="mapNode" class="GeoMapVisualization" @pointerdown.stop @wheel.stop.passive></div>
</template>

<style scoped>
.GeoMapVisualization {
  height: 100%;
}

:deep(.tooltip) {
  & > .mapboxgl-popup-content {
    background-color: rgb(252, 250, 245);
    font-size: 12px;
    border-radius: 14px;
    border-top-left-radius: 2px;
    font-family: DejaVuSansMonoBook, sans-serif;
    color: rgba(0, 0, 0, 0.8);
    border: 1px solid rgb(200, 210, 210);
    /* This is required for it to show above Mapbox's information button.*/
    z-index: 2;
  }

  & > .mapboxgl-popup-tip {
    display: none;
  }
}

:deep(.mapboxgl-map) {
  border-radius: var(--radius-default);
}

:deep(.mapboxgl-ctrl-attrib.mapboxgl-compact) {
  min-height: 24px;
}

/* Copied from mapbox-gl CSS. This is required because Tailwind's global CSS reset resets
 * `background-color` and `background-image` to default values for buttons, and Mapbox's
 * selector has lower specificity. */
:deep(.mapboxgl-ctrl-attrib-button.mapboxgl-ctrl-attrib-button) {
  background-color: hsla(0, 0%, 100%, 0.5);
  background-image: url("data:image/svg+xml;charset=utf-8,%3Csvg width='24' height='24' viewBox='0 0 20 20' xmlns='http://www.w3.org/2000/svg' fill-rule='evenodd'%3E%3Cpath d='M4 10a6 6 0 1012 0 6 6 0 10-12 0m5-3a1 1 0 102 0 1 1 0 10-2 0m0 3a1 1 0 112 0v3a1 1 0 11-2 0'/%3E%3C/svg%3E");
}
</style>
