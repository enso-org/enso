/** @file Constants shared between all service workers (development and production). */
import * as common from 'enso-common'

// =================
// === Constants ===
// =================

/** The name of the cache under which offline assets are stored. */
export const CACHE_NAME = common.PRODUCT_NAME.toLowerCase()

/** The numbers after each font loaded by the "M PLUS 1" font. */
const M_PLUS_1_SECTIONS = [
    /* eslint-disable @typescript-eslint/no-magic-numbers */
    0, 1, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27,
    28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 53,
    54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77,
    78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100,
    101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
    /* eslint-enable @typescript-eslint/no-magic-numbers */
]

/** The complete list of assets to cache for offline use. */
export const DEPENDENCIES = [
    // app/gui/view/graph-editor/src/builtin/visualization/java_script/heatmap.js
    // app/gui/view/graph-editor/src/builtin/visualization/java_script/histogram.js
    // app/gui/view/graph-editor/src/builtin/visualization/java_script/scatterPlot.js
    'https://d3js.org/d3.v4.min.js',
    'https://fonts.cdnfonts.com/css/dejavu-sans-mono',
    // Loaded by https://fonts.cdnfonts.com/css/dejavu-sans-mono
    'https://fonts.cdnfonts.com/s/108/DejaVuSansMono.woff',
    'https://fonts.cdnfonts.com/s/108/DejaVuSansMono-Oblique.woff',
    'https://fonts.cdnfonts.com/s/108/DejaVuSansMono-Bold.woff',
    'https://fonts.cdnfonts.com/s/108/DejaVuSansMono-BoldOblique.woff',
    // app/gui/view/graph-editor/src/builtin/visualization/java_script/geoMap.js
    'https://unpkg.com/deck.gl@8.4/dist.min.js',
    'https://api.mapbox.com/mapbox-gl-js/v2.1.1/mapbox-gl.js',
    'https://api.mapbox.com/mapbox-gl-js/v2.1.1/mapbox-gl.css',
    // Loaded by https://api.mapbox.com/mapbox-gl-js/v2.1.1/mapbox-gl.js
    'https://api.mapbox.com/styles/v1/mapbox/light-v9?access_token=pk.' +
        'eyJ1IjoiZW5zby1vcmciLCJhIjoiY2tmNnh5MXh2MGlyOTJ5cWdubnFxbXo4ZSJ9.3KdAcCiiXJcSM18nwk09-Q',
    'https://api.mapbox.com/styles/v1/mapbox/light-v9/sprite.json?access_token=pk.' +
        'eyJ1IjoiZW5zby1vcmciLCJhIjoiY2tmNnh5MXh2MGlyOTJ5cWdubnFxbXo4ZSJ9.3KdAcCiiXJcSM18nwk09-Q',
    'https://api.mapbox.com/styles/v1/mapbox/light-v9/sprite.png?access_token=pk.' +
        'eyJ1IjoiZW5zby1vcmciLCJhIjoiY2tmNnh5MXh2MGlyOTJ5cWdubnFxbXo4ZSJ9.3KdAcCiiXJcSM18nwk09-Q',
    // app/gui/view/graph-editor/src/builtin/visualization/java_script/sql.js
    'https://cdnjs.cloudflare.com/ajax/libs/sql-formatter/4.0.2/sql-formatter.min.js',
    // app/gui/view/graph-editor/src/builtin/visualization/java_script/table.js
    'https://cdn.jsdelivr.net/npm/ag-grid-community/dist/ag-grid-community.min.js',
    'https://cdn.jsdelivr.net/npm/ag-grid-community/styles/ag-grid.css',
    'https://cdn.jsdelivr.net/npm/ag-grid-community/styles/ag-theme-alpine.css',
    // app/ide-desktop/lib/content/src/docsStyle.css
    'https://fonts.gstatic.com/s/sourcecodepro/v14/HI_XiYsKILxRpg3hIP6sJ7fM7PqtlsnztA.ttf',
    'https://fonts.gstatic.com/s/sourcecodepro/v14/HI_SiYsKILxRpg3hIP6sJ7fM7PqVOg.ttf',
    'https://fonts.gstatic.com/s/sourcecodepro/v14/HI_XiYsKILxRpg3hIP6sJ7fM7PqtzsjztA.ttf',
    'https://fonts.gstatic.com/s/sourcecodepro/v14/HI_XiYsKILxRpg3hIP6sJ7fM7Pqt4s_ztA.ttf',
    'https://fonts.gstatic.com/s/sourcecodepro/v14/HI_XiYsKILxRpg3hIP6sJ7fM7Pqths7ztA.ttf',
    // app/ide-desktop/lib/dashboard/src/tailwind.css
    'https://fonts.googleapis.com/css2?family=M+PLUS+1:wght@500;700&display=swap',
    // Loaded by https://fonts.googleapis.com/css2?family=M+PLUS+1:wght@500;700&display=swap
    ...M_PLUS_1_SECTIONS.map(
        number =>
            `https://fonts.gstatic.com/s/mplus1/v6/` +
            `R70ZjygA28ymD4HgBVu92j6eR1mYP_TX-Bb-rTg93gHfHe9F4Q.${number}.woff2`
    ),
]
