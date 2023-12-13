/** @file Utilities to work with the host environment, whether it is a browser of node. */

import { logger } from 'runner/logger'

// ======================
// === Host Utilities ===
// ======================

/** Resolves to `true` if we are running the script in the browser. If it's node, resolves to
 * `false`. */
export const isBrowser = typeof window !== 'undefined'

/** Resolves to `true` if we are running the script in node. If it's a browser, resolves to
 * `false`. */
export const isNode = !isBrowser

export interface UrlParams {
  [key: string]: string | UrlParams
}

/** Returns the parameters passed in the URL query string. */
export function urlParams(): UrlParams {
  if (isBrowser) {
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
export function exportGlobal(exports: Record<string, any>) {
  for (const [key, value] of Object.entries(exports)) {
    /* eslint @typescript-eslint/no-unsafe-assignment: "off" */
    // @ts-expect-error
    globalThis[key] = value
  }
}
