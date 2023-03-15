/** @file Utilities to work with the host environment, whether it is a browser of node. */

import { logger } from './log'

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
 * `global` object.
 *
 * The `no-unnecessary-condition` lint fires a false positive when `global` is not `undefined`. This
 * it the case when running the script in node. */
// const global = {}
// eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
global ??= window

interface UrlParams {
    [key: string]: string | UrlParams
}

/** Returns the parameters passed in the URL query string. */
function urlParams(): UrlParams {
    if (browser) {
        const out: UrlParams = {}
        const urlParams = new URLSearchParams(window.location.search)
        for (const [name, value] of urlParams.entries()) {
            let obj = out
            const path = name.split('.')
            const lastSegment = path.pop()
            if (lastSegment == null) {
                logger.error(`Invalid URL parameter name: '${name}'`)
            } else {
                let segment = null
                while ((segment = path.shift()) != null) {
                    const nextObj = obj[segment] ?? {}
                    if (typeof nextObj === 'string') {
                        logger.error(`Duplicate URL parameter name: '${name}'`)
                    } else {
                        obj[segment] = nextObj
                        obj = nextObj
                    }
                }
                obj[lastSegment] = value
            }
        }
        return out
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
