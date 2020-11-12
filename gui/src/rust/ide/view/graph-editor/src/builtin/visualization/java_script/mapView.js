/** GeoMap Visualization. */

// =================
// === Constants ===
// =================

/**
 * Mapbox API access token.
 * All the limits of API are listed here: https://docs.mapbox.com/api/#rate-limits
 */
const TOKEN = 'pk.eyJ1IjoiZW5zby1vcmciLCJhIjoiY2tmNnh5MXh2MGlyOTJ5cWdubnFxbXo4ZSJ9.3KdAcCiiXJcSM18nwk09-Q';

const GEO_POINT            = "Geo_Point";
const GEO_MAP              = "Geo_Map";
const SCATTERPLOT_LAYER    = "Scatterplot_Layer";
const DEFAULT_POINT_RADIUS = 150;
const DEFAULT_MAP_ZOOM     = 11;
const DARK_ACCENT_COLOR    = [222,162,47];
const LIGHT_ACCENT_COLOR   = [1,234,146];


function loadScript(url) {
    var script = document.createElement("script");
    script.src = url;

    document.head.appendChild(script);
}

function loadStyle(url) {
    var link  = document.createElement("link");
    link.href = url;
    link.rel  = 'stylesheet';

    document.head.appendChild(link);
}

loadScript('https://unpkg.com/deck.gl@latest/dist.min.js');
loadScript('https://api.tiles.mapbox.com/mapbox-gl-js/v1.6.1/mapbox-gl.js');
loadStyle('https://api.tiles.mapbox.com/mapbox-gl-js/v1.6.1/mapbox-gl.css');

const styleHead = document.createElement("style")
styleHead.innerText = `.mapboxgl-map {
            border-radius: 14px;
        }`
document.head.appendChild(styleHead);


/**
 * Provides a mapbox & deck.gl-based map visualization for IDE.
 *
 * > Example creates a map with described properties with a scatter plot overlay:
 * {
 * "type": "Geo_Map",
 * "latitude": 37.8,
 * "longitude": -122.45,
 * "zoom": 15,
 * "controller": true,
 * "layers": [{
 *     "type": "Scatterplot_Layer",
 *     "data": [{
 *         "type": "Geo_Point",
 *         "latitude": -122.45,
 *         "longitude": 37.8,
 *         "color": [255, 0, 0],
 *         "radius": 100
 *     }]
 * }]
 * }
 */
class MapViewVisualization extends Visualization {
    static inputType = "Any"

    onDataReceived(data) {
        while (this.dom.firstChild) {
            this.dom.removeChild(this.dom.lastChild);
        }

        const width   = this.dom.getAttributeNS(null,"width");
        const height  = this.dom.getAttributeNS(null,"height");
        const mapElem = document.createElement("div");
        mapElem.setAttributeNS(null,"id"   ,"map");
        mapElem.setAttributeNS(null,"style","width:" + width + "px;height: " + height + "px;");
        this.dom.appendChild(mapElem);

        let parsedData = data;
        if (typeof data === "string") {
            parsedData = JSON.parse(data);
        }

        let defaultMapStyle = 'mapbox://styles/mapbox/light-v9';
        let accentColor     = LIGHT_ACCENT_COLOR;
        if (document.getElementById("root").classList.contains("dark")){
            defaultMapStyle = 'mapbox://styles/mapbox/dark-v9';
            accentColor     = DARK_ACCENT_COLOR;
        }

        let preparedDataPoints = []
        let computed           = this.prepareDataPoints(parsedData,preparedDataPoints,accentColor);
        const scatterplotLayer = new deck.ScatterplotLayer({
            data: preparedDataPoints,
            getFillColor:d => d.color,
            getRadius:d => d.radius
        })

        let latitude   = ok(parsedData.latitude)   ? parsedData.latitude : computed.latitude;
        let longitude  = ok(parsedData.longitude)  ? parsedData.longitude : computed.longitude;
        // TODO : Compute zoom somehow from span of latitudes and longitudes.
        let zoom       = ok(parsedData.zoom)       ? parsedData.zoom : DEFAULT_MAP_ZOOM;
        let mapStyle   = ok(parsedData.mapStyle)   ? parsedData.mapStyle : defaultMapStyle;
        let pitch      = ok(parsedData.pitch)      ? parsedData.pitch : 0;
        let controller = ok(parsedData.controller) ? parsedData.controller : true;

        const deckgl = new deck.DeckGL({
            container: 'map',
            mapboxApiAccessToken: TOKEN,
            mapStyle,
            initialViewState: {longitude,latitude,zoom,pitch},
            controller
        });

        deckgl.setProps({
            layers: [scatterplotLayer]
        });
    }

    /**
     * Prepares data points to be shown on the map.
     *
     * It checks the type of input data, whether user wants to display single `GEO_POINT`, array of
     * those, `SCATTERPLOT_LAYER` or a fully defined `GEO_MAP`, and prepares data field of deck.gl
     * layer for given input.
     *
     * @param preparedDataPoints - List holding data points to push the GeoPoints into.
     * @param parsedData         - All the parsed data to create points from.
     * @param accentColor        - accent color of IDE if element doesn't specify one.
     */
    prepareDataPoints(parsedData, preparedDataPoints, accentColor) {
        let latitude  = 0.0;
        let longitude = 0.0;

        if (parsedData.type === GEO_POINT) {
            this.pushGeoPoint(preparedDataPoints,parsedData,accentColor);
            latitude  = parsedData.latitude;
            longitude = parsedData.longitude;
        } else if (Array.isArray(parsedData) && parsedData.length) {
            const computed = this.calculateExtremesAndPushPoints(parsedData,preparedDataPoints,accentColor);
            latitude       = computed.latitude;
            longitude      = computed.longitude;
        } else {
            if (parsedData.type === SCATTERPLOT_LAYER && parsedData.data.length) {
                const computed = this.calculateExtremesAndPushPoints(parsedData.data,preparedDataPoints,accentColor);
                latitude       = computed.latitude;
                longitude      = computed.longitude;
            } else if (parsedData.type === GEO_MAP && ok(parsedData.layers)) {
                parsedData.layers.forEach(layer => {
                    if (layer.type === SCATTERPLOT_LAYER) {
                        let dataPoints = layer.data || [];
                        const computed = this.calculateExtremesAndPushPoints(dataPoints,preparedDataPoints,accentColor);
                        latitude       = computed.latitude;
                        longitude      = computed.longitude;
                    } else {
                        console.warn("Geo_Map: Currently unsupported deck.gl layer.")
                    }
                })
            }
        }
        return {latitude,longitude}
    }

    /**
     * Helper for prepareDataPoints, pushes `GEO_POINT`'s to the list, and calculates central point.
     * @returns {{latitude: number, longitude: number}} - center.
     */
    calculateExtremesAndPushPoints(dataPoints, preparedDataPoints, accentColor) {
        let latitudes  = [];
        let longitudes = [];
        dataPoints.forEach(e => {
            if(e.type === GEO_POINT) {
                this.pushGeoPoint(preparedDataPoints,e,accentColor);
                latitudes.push(e.latitude);
                longitudes.push(e.longitude);
            }
        });
        let latitude  = 0.0;
        let longitude = 0.0;
        if (latitudes.length && longitudes.length) {
            let minLat = Math.min.apply(null,latitudes);
            let maxLat = Math.max.apply(null,latitudes);
            latitude   = (minLat + maxLat)/2;
            let minLon = Math.min.apply(null,longitudes);
            let maxLon = Math.max.apply(null,longitudes);
            longitude  = (minLon + maxLon)/2;
        }
        return {latitude,longitude};
    }

    /**
     * Pushes a new deck.gl-compatible point made out of `GEO_POINT`
     *
     * @param preparedDataPoints - List holding geoPoints to push the new element into.
     * @param geoPoint           - `GEO_POINT` to create new deck.gl point from.
     * @param accentColor        - accent color of IDE if `GEO_POINT` doesn't specify one.
     */
    pushGeoPoint(preparedDataPoints,geoPoint,accentColor) {
        let position = [geoPoint.longitude,geoPoint.latitude];
        let radius   = isNaN(geoPoint.radius)  ? DEFAULT_POINT_RADIUS : geoPoint.radius;
        let color    = ok(geoPoint.color) ? geoPoint.color : accentColor;
        preparedDataPoints.push({position,color,radius});
    }

    /**
     * Sets size of the visualization.
     * @param size - new size, list of two numbers containing width and height respectively.
     */
    setSize(size) {
        this.dom.setAttributeNS(null,"width",size[0]);
        this.dom.setAttributeNS(null,"height",size[1]);
    }
}

/**
 * Checks if `t` has defined type and is not null.
 */
function ok(t) {
    return t !== undefined && t !== null;
}

return MapViewVisualization;
