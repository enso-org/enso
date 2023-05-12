/** @file A service worker that redirects paths without extensions to `/index.html`.
 * This is required for paths like `/login`, which are handled by client-side routing,
 * to work when developing locally on `localhost:8080`. */
// Bring globals and interfaces specific to Web Workers into scope.
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
        event.respondWith(responsePromise)
        return
    } else {
        return false
    }
})
