<script lang="ts">
export const name = 'Geo Map'
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
const MAPBOX_ID_PREFIX = 'vis-data-'

type Data = RegularData | Layer | DataFrame

interface RegularData {
  layers: Layer[]
}

interface ScatterplotLayer {
  type: 'Scatterplot_Layer'
  data: Location[]
}

interface GeoJsonLayer {
  type: 'GeoJsonLayer'
  data: GeoJSON.GeoJSON | GeoJSON.Geometry[]
}

type Layer = ScatterplotLayer | GeoJsonLayer

type Color = string

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
import bbox from '@turf/bbox'
import {
  computed,
  effectScope,
  onMounted,
  onScopeDispose,
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

const dataAsGeoJSONs = computed(() => {
  if ('type' in props.data) {
    return [layerToGeoJSON(props.data)]
  } else if ('df_latitude' in props.data) {
    return [dataframeToGeoJSON(props.data)]
  } else {
    return props.data.layers.map(layerToGeoJSON)
  }
})

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
      if (layer.data instanceof Array) {
        return {
          type: 'GeometryCollection',
          geometries: layer.data,
        }
      } else {
        return layer.data
      }
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

function boundingBoxOfCurrentData() {
  let finalBBox: mapboxgl.LngLatBounds | undefined
  for (const geojson of dataAsGeoJSONs.value) {
    const layerBbox = bbox(geojson)
    // `bbox` sometimes returns 3d box.
    const layerBboxFlat: [number, number, number, number] =
      layerBbox.length == 4 ? layerBbox : [layerBbox[0], layerBbox[1], layerBbox[3], layerBbox[4]]
    finalBBox = finalBBox?.extend(layerBboxFlat) ?? new mapboxgl.LngLatBounds(layerBboxFlat)
  }
  return finalBBox
}

const scope = effectScope()

class GeoMapVisualizationMap {
  map: mapboxgl.Map
  tooltip: mapboxgl.Popup
  /** Mapbox sources currently attached to {@link map} */
  private mapSources: string[] = []
  /** Mapbox layers currently attached to {@link map} */
  private mapLayers: string[] = []

  /**
   * Create and initialize Mapbox GL map, and keep it up-to-date with {@link dataAsGeoJSONs}
   *
   * The lifetime of the map is bound to current component scope; when scope is stopped, the
   * underlying mapboxgl.Map instance is destroyed.
   */
  constructor(container: HTMLElement) {
    this.map = new mapboxgl.Map({
      container,
      projection: 'mercator',
      zoom: DEFAULT_MAP_ZOOM,
    })
    scope.run(() => onScopeDispose(() => this.map.remove()))
    this.tooltip = this.setupTooltip()
    // Hotfix for https://github.com/mapbox/mapbox-gl-js/issues/13355
    ;(this.map as any)._updateContainerDimensions = function () {
      if (!this._container) return

      const width = this._container.offsetWidth || 400
      const height = this._container.offsetHeight || 300

      this._containerWidth = width
      this._containerHeight = height
    }
    this.map.on('style.load', () => {
      // This is for suppressing "Cutoff is currently disabled on terrain"
      // warning (and enabling better polygon rendering).
      this.map.setTerrain(null)
    })
    this.map.on('load', () => {
      this.updateMap()
      scope.run(() => watch(dataAsGeoJSONs, () => this.updateMap()))
    })
  }

  private setupTooltip() {
    const popup = new mapboxgl.Popup({
      anchor: 'top-left',
      closeButton: false,
      closeOnClick: false,
      className: 'tooltip',
      offset: 4,
    })

    this.map.on('style.load', () => {
      // queryRenderedFeatures works only on loaded styles (otherwise we get exceptions).
      this.map.on('mousemove', (event) => {
        const feature = this.map.queryRenderedFeatures(event.point)[0]
        if (feature?.properties?.label) {
          popup.setLngLat(event.lngLat).setText(feature.properties.label).addTo(this.map)
        } else {
          popup.remove()
        }
      })
      this.map.on('mouseout', () => popup.remove())
    })

    return popup
  }

  /** Update map state to current {@link dataAsGeoJSONs}. */
  updateMap() {
    this.removeAllSourcesAndLayers()
    this.addSources()
    this.addFillLayer()
    this.addLineLayer()
    this.addCircleLayer()

    const bounds = boundingBoxOfCurrentData()
    if (bounds != null) {
      this.map.fitBounds(bounds, {
        padding: FIT_PADDING,
        maxZoom: DEFAULT_MAX_MAP_ZOOM,
        duration: 500,
      })
    }
  }

  private removeAllSourcesAndLayers() {
    for (const oldLayer of this.mapLayers) {
      this.map.removeLayer(oldLayer)
    }
    for (const oldSources of this.mapSources) {
      this.map.removeSource(oldSources)
    }
    this.mapLayers.length = this.mapSources.length = 0
  }

  private addSources() {
    dataAsGeoJSONs.value.forEach((geojson, index) => {
      const sourceId = `${MAPBOX_ID_PREFIX}${index}`
      this.map.addSource(sourceId, {
        type: 'geojson',
        data: geojson,
      })
      this.mapSources.push(sourceId)
    })
  }

  private addFillLayer() {
    for (const sourceId of this.mapSources) {
      const polygonsLayerId = `${sourceId}-polygons`
      this.map.addLayer({
        id: polygonsLayerId,
        type: 'fill',
        source: sourceId,
        paint: {
          'fill-color': ['coalesce', ['get', 'color'], DEFAULT_COLOR],
          'fill-outline-color': ['coalesce', ['get', 'color'], DEFAULT_COLOR],
          'fill-opacity': 0.3,
        },
        filter: ['in', ['geometry-type'], ['literal', ['Polygon', 'MultiPolygon']]],
      })
      this.mapLayers.push(polygonsLayerId)
    }
  }

  private addLineLayer() {
    for (const sourceId of this.mapSources) {
      const linesLayerId = `${sourceId}-lines`
      this.map.addLayer({
        id: linesLayerId,
        type: 'line',
        source: sourceId,
        paint: {
          'line-color': ['coalesce', ['get', 'color'], DEFAULT_COLOR],
        },
        filter: [
          'in',
          ['geometry-type'],
          ['literal', ['LineString', 'MultiLineString', 'Polygon', 'MultiPolygon']],
        ],
      })
      this.mapLayers.push(linesLayerId)
    }
  }

  private addCircleLayer() {
    for (const sourceId of this.mapSources) {
      const pointsLayerId = `${sourceId}-points`
      this.map.addLayer({
        id: pointsLayerId,
        type: 'circle',
        source: sourceId,
        paint: {
          'circle-radius': ['coalesce', ['get', 'radius'], DEFAULT_RADIUS],
          'circle-color': ['coalesce', ['get', 'color'], DEFAULT_COLOR],
        },
        filter: ['==', ['geometry-type'], 'Point'],
      })
      this.mapLayers.push(pointsLayerId)
    }
  }
}

onMounted(() => {
  if (mapNode.value == null) {
    console.error('Cannot initialize MapBoxGL: no container element!')
    return
  }
  const map = new GeoMapVisualizationMap(mapNode.value)
  scope.run(() =>
    watch(
      () => config.size,
      () => map.map.resize(),
    ),
  )
})
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
