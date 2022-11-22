/** GeoMap Visualization. */

// =================
// === Constants ===
// =================

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
const LABEL_DARK_BACKGROUND = `rgb(93, 91, 88)`
const LABEL_LIGHT_BACKGROUND = `rgb(252, 250, 245)`
const LABEL_DARK_OUTLINE = `rgb(52, 50, 48)`
const LABEL_LIGHT_OUTLINE = `rgb(200, 210, 210)`
const LABEL_DARK_COLOR = `rgba(255, 255, 255, 0.8)`
const LABEL_LIGHT_COLOR = `rgba(0, 0, 0, 0.8)`

const DEFAULT_MAP_ZOOM = 11
const DARK_ACCENT_COLOR = [222, 162, 47]
const LIGHT_ACCENT_COLOR = [78, 165, 253]

// =====================================
// === Script & Style Initialisation ===
// =====================================

loadScript('https://unpkg.com/deck.gl@8.4/dist.min.js')
loadScript('https://api.mapbox.com/mapbox-gl-js/v2.1.1/mapbox-gl.js')
loadStyle('https://api.mapbox.com/mapbox-gl-js/v2.1.1/mapbox-gl.css')

const mapboxStyle = `
.mapboxgl-map {
 border-radius: 14px;
}`

loadStyleFromString(mapboxStyle)

// ====================
// === Id Generator ===
// ====================

/**
 * Returns a function that will generate unique map ids of the form "map_" followed by a number.
 * Each call to the returned function will return a unique id.
 *
 * @returns {function(): string}
 */
function makeGenerator() {
    let _id = 0
    return () => {
        const id = 'map_' + _id.toString()
        _id += 1
        return id
    }
}

const makeId = makeGenerator()

// ============================
// === MapViewVisualization ===
// ============================

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
class GeoMapVisualization extends Visualization {
    static inputType = 'Standard.Table.Data.Table.Table'
    static label = 'Geo Map'

    constructor(api) {
        super(api)
        this.initMapElement()
        this.initStyle()
        this.dataPoints = []
        this.setPreprocessor('Standard.Visualization.Geo_Map', 'process_to_json_text')
    }

    initMapElement() {
        const width = this.dom.getAttributeNS(null, 'width')
        const height = this.dom.getAttributeNS(null, 'height')
        const mapElem = document.createElement('div')
        this.mapId = makeId()
        mapElem.setAttributeNS(null, 'id', this.mapId)
        mapElem.setAttributeNS(null, 'style', 'width:' + width + 'px;height: ' + height + 'px;')
        this.dom.appendChild(mapElem)
        this.mapElem = mapElem
    }

    initStyle() {
        let defaultMapStyle = 'mapbox://styles/mapbox/light-v9'
        let accentColor = LIGHT_ACCENT_COLOR
        let labelBackgroundColor = LABEL_LIGHT_BACKGROUND
        let labelColor = LABEL_LIGHT_COLOR
        let labelOutline = LABEL_LIGHT_OUTLINE
        if (document.getElementById('root').classList.contains('dark-theme')) {
            defaultMapStyle = 'mapbox://styles/enso-org/ckiu0o0in2fpp19rpk0jfvg2s'
            accentColor = DARK_ACCENT_COLOR
            labelBackgroundColor = LABEL_DARK_BACKGROUND
            labelColor = LABEL_DARK_COLOR
            labelOutline = LABEL_DARK_OUTLINE
        }
        this.defaultMapStyle = defaultMapStyle
        this.accentColor = accentColor
        this.labelBackgroundColor = labelBackgroundColor
        this.labelColor = labelColor
        this.labelOutline = labelOutline
    }

    onDataReceived(data) {
        let parsedData = data
        if (typeof data === 'string') {
            parsedData = JSON.parse(data)
        }
        if (this.updateState(parsedData)) {
            this.updateMap()
            this.updateLayers()
        }
    }

    /**
     * Update the internal data with the new incoming data. Does not affect anything rendered.
     * Returns true on a successful update and false if no valid data was provided.
     */
    updateState(data) {
        // For now we assume every update has all data. If we move to incremental updates we need
        // to keep the old state and do a proper update.
        this.resetState()

        extractDataPoints(data, this.dataPoints, this.accentColor)
        if (this.dataPoints.length === 0) {
            // We have no valid data and should skip initialization.
            return false
        }
        const { latitude, longitude } = this.centerPoint()

        this.latitude = data.latitude ?? latitude
        this.longitude = data.longitude ?? longitude
        // TODO : Compute zoom somehow from span of latitudes and longitudes.
        this.zoom = data.zoom ?? DEFAULT_MAP_ZOOM
        this.mapStyle = data.mapStyle ?? this.defaultMapStyle
        this.pitch = data.pitch ?? 0
        this.controller = data.controller ?? true
        this.showingLabels = data.showingLabels ?? false
        return true
    }

    viewState() {
        return {
            longitude: this.longitude,
            latitude: this.latitude,
            zoom: this.zoom,
            pitch: this.pitch,
        }
    }

    updateMap() {
        if (!ok(this.deckgl)) {
            this.initDeckGl()
        } else {
            this.updateDeckGl()
        }
    }

    makeScatterLayer() {
        return new deck.ScatterplotLayer({
            data: this.dataPoints,
            getFillColor: d => d.color,
            getRadius: d => d.radius,
            pickable: this.showingLabels,
        })
    }

    initDeckGl() {
        try {
            this.deckgl = new deck.DeckGL({
                container: this.mapId,
                mapboxApiAccessToken: TOKEN,
                mapStyle: this.mapStyle,
                initialViewState: this.viewState(),
                controller: this.controller,
            })
        } catch (error) {
            console.error(error)
            this.resetState()
            this.resetDeckGl()
        }
    }

    /**
     * Reset the internal state of the visualization, discarding all previous data updates.
     */
    resetState() {
        // We only need to reset the data points as everything else will be overwritten when new
        // data arrives.
        this.dataPoints = []
    }

    resetDeckGl() {
        this.deckgl = undefined
        this.resetMapElement()
    }

    resetMapElement() {
        while (this.mapElem.hasChildNodes()) {
            this.mapElem.removeChild(this.mapElem.childNodes[0])
        }
    }

    updateDeckGl() {
        this.deckgl.viewState = this.viewState()
        this.deckgl.mapStyle = this.mapStyle
        this.deckgl.controller = this.controller
    }

    updateLayers() {
        if (!ok(this.deckgl)) {
            return
        }
        this.deckgl.setProps({
            layers: [this.makeScatterLayer()],
            getTooltip: ({ object }) =>
                object && {
                    html: `<div>${object.label}</div>`,
                    style: {
                        backgroundColor: this.labelBackgroundColor,
                        fontSize: LABEL_FONT_SIZE,
                        borderRadius: LABEL_BORDER_RADIUS,
                        borderTopLeftRadius: LABEL_BORDER_TOP_LEFT_RADIUS,
                        fontFamily: LABEL_FONT,
                        margin: LABEL_MARGIN,
                        color: this.labelColor,
                        border: '1px solid ' + this.labelOutline,
                    },
                },
        })
    }

    centerPoint() {
        const { x, y } = calculateCenterPoint(this.dataPoints)
        return { latitude: y, longitude: x }
    }

    /**
     * Sets size of the visualization.
     * @param size - new size, list of two numbers containing width and height respectively.
     */
    setSize(size) {
        this.dom.setAttributeNS(null, 'width', size[0])
        this.dom.setAttributeNS(null, 'height', size[1])
        this.mapElem.setAttributeNS(
            null,
            'style',
            'width:' + size[0] + 'px;height: ' + size[1] + 'px;'
        )
    }
}

/**
 * Extract the visualization data from a full configuration object.
 */
function extractVisualizationDataFromFullConfig(parsedData, preparedDataPoints, accentColor) {
    if (parsedData.type === SCATTERPLOT_LAYER && parsedData.data.length) {
        pushPoints(parsedData.data, preparedDataPoints, accentColor)
    } else if (ok(parsedData.layers)) {
        parsedData.layers.forEach(layer => {
            if (layer.type === SCATTERPLOT_LAYER) {
                let dataPoints = layer.data ?? []
                pushPoints(dataPoints, preparedDataPoints, accentColor)
            } else {
                console.warn('Geo_Map: Currently unsupported deck.gl layer.')
            }
        })
    }
}

/**
 * Extract the visualization data from a dataframe.
 */
function extractVisualizationDataFromDataFrame(parsedData, preparedDataPoints, accentColor) {
    const geoPoints = parsedData.df_latitude.map(function (latitude, i) {
        const longitude = parsedData.df_longitude[i]
        const label = parsedData.df_label?.[i]
        const color = parsedData.df_color?.[i]
        const radius = parsedData.df_radius?.[i]
        return { latitude, longitude, label, color, radius }
    })
    pushPoints(geoPoints, preparedDataPoints, accentColor)
}

function isDataFrame(data) {
    return data.df_latitude !== undefined && data.df_longitude !== undefined
}

/**
 * Extracts the data form the given `parsedData`. Checks the type of input data and prepares our
 * internal data  (`GeoPoints') for consumption in deck.gl.
 *
 * @param parsedData         - All the parsed data to create points from.
 * @param preparedDataPoints - List holding data points to push the GeoPoints into.
 * @param accentColor        - accent color of IDE if element doesn't specify one.
 */
function extractDataPoints(parsedData, preparedDataPoints, accentColor) {
    if (isDataFrame(parsedData)) {
        extractVisualizationDataFromDataFrame(parsedData, preparedDataPoints, accentColor)
    } else {
        extractVisualizationDataFromFullConfig(parsedData, preparedDataPoints, accentColor)
    }
}

/**
 * Transforms the `dataPoints` to the internal data format and appends them to the `targetList`.
 * Also adds the `accentColor` for each point.
 *
 * Expects the `dataPoints` to be a list of objects that have a `longitude` and `latitude` and
 * optionally `radius`, `color` and `label`.
 */
function pushPoints(dataPoints, targetList, accentColor) {
    dataPoints.forEach(geoPoint => {
        if (isValidNumber(geoPoint.longitude) && isValidNumber(geoPoint.latitude)) {
            let position = [geoPoint.longitude, geoPoint.latitude]
            let radius = isValidNumber(geoPoint.radius) ? geoPoint.radius : DEFAULT_POINT_RADIUS
            let color = geoPoint.color ?? accentColor
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
    dataPoints.forEach(e => {
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
 * @returns {{x: number, y: number}}
 */
function calculateCenterPoint(dataPoints) {
    let { minX, maxX, minY, maxY } = calculateExtent(dataPoints)
    let x = (minX + maxX) / 2
    let y = (minY + maxY) / 2
    return { x, y }
}

/**
 * Checks if `t` has defined type and is not null.
 */
function ok(t) {
    return t !== undefined && t !== null
}

return GeoMapVisualization
