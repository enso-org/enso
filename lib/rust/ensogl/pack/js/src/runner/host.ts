/** @file Utilities to work with the host environment, whether it is a browser of node. */

// ======================
// === Host Utilities ===
// ======================

/** Resolves to `true` if we are running the script in the browser. If it's node, resolves to
 * `false`. */
const browser = typeof window !== 'undefined'

/** Resolves to `true` if we are running the script in node. If it's a browser, resolves to
 * `false`. */
const node = !browser

/** The global object. In case of a browser, this is the window. In case of node, it's the built-in
 * `global` object. */
// const global = {}
global ??= window

/** Returns the parameters passed in the URL query string. */
function urlParams(): Record<string, any> {
    if (browser) {
        const urlParams = new URLSearchParams(window.location.search)
        return Object.fromEntries(urlParams.entries())
    } else {
        return {}
    }
}

/** Export the value to the global scope (`global` in node and `window` in browser). */
function exportGlobal(exports: Record<string, any>) {
    for (const [key, value] of Object.entries(exports)) {
        /* eslint @typescript-eslint/no-unsafe-assignment: "off" */
        // @ts-expect-error
        global[key] = value
    }
}

export default {
    global,
    exportGlobal,
    browser,
    node,
    urlParams,
}
