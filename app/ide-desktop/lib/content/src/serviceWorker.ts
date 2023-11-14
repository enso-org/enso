/** @file A service worker that redirects paths without extensions to `/index.html`.
 * This is required for paths like `/login`, which are handled by client-side routing,
 * to work when developing locally on `localhost:8080`. */
// Bring globals and interfaces specific to Web Workers into scope.
/// <reference lib="WebWorker" />
import * as constants from './serviceWorkerConstants'

// =====================
// === Fetch handler ===
// =====================

// We `declare` a variable here because Service Workers have a different global scope.
// eslint-disable-next-line no-restricted-syntax
declare const self: ServiceWorkerGlobalScope

self.addEventListener('install', event => {
    event.waitUntil(
        caches.open(constants.CACHE_NAME).then(cache => {
            void cache.addAll(constants.DEPENDENCIES)
            return
        })
    )
})

self.addEventListener('fetch', event => {
    const url = new URL(event.request.url)
    if (url.hostname === 'localhost' || url.hostname === '127.0.0.1') {
        return false
    } else {
        event.respondWith(
            caches
                .open(constants.CACHE_NAME)
                .then(cache => cache.match(event.request))
                .then(response => response ?? fetch(event.request))
        )
        return
    }
})
