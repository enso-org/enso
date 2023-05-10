/** @file A service worker that redirects paths without extensions to `/index.html`.
 * This is only used in the cloud frontend. */
/// <reference lib="WebWorker" />
import * as common from 'enso-common'

// =====================
// === Fetch handler ===
// =====================

// We `declare` a variable here because Service Workers have a different global scope.
// eslint-disable-next-line no-restricted-syntax
declare const self: ServiceWorkerGlobalScope

self.addEventListener('fetch', event => {
    const url = new URL(event.request.url)
    if (url.hostname === 'localhost' && url.pathname !== '/esbuild') {
        const responsePromise = /\/[^.]+$/.test(event.request.url)
            ? fetch('/index.html')
            : fetch(event.request.url)
        event.respondWith(
            responsePromise.then(response => {
                const clonedResponse = new Response(response.body, response)
                for (const [header, value] of common.COOP_COEP_CORP_HEADERS) {
                    clonedResponse.headers.set(header, value)
                }
                return clonedResponse
            })
        )
        return
    } else {
        return false
    }
})
