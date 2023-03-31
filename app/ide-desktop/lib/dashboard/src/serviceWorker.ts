/** @file A service worker that redirects paths without extensions to `/index.html`. */
/// <reference lib="WebWorker" />

// We `declare` a variable here because Service Workers have a different global scope.
// eslint-disable-next-line no-restricted-syntax
declare const self: ServiceWorkerGlobalScope

self.addEventListener('fetch', event => {
    const url = new URL(event.request.url)
    if (
        url.hostname === 'localhost' &&
        /\/[^.]+$/.test(event.request.url) &&
        url.pathname !== '/esbuild'
    ) {
        event.respondWith(fetch('/index.html'))
        return
    } else {
        return false
    }
})

// Required for TypeScript to consider it a module, instead of in window scope.
export {}
