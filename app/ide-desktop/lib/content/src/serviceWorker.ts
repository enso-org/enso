/** @file A service worker that redirects paths without extensions to `/index.html`.
 * This is only used in the cloud frontend. */
/// <reference lib="WebWorker" />

// =====================
// === Fetch handler ===
// =====================

// We `declare` a variable here because Service Workers have a different global scope.
// eslint-disable-next-line no-restricted-syntax
declare const self: ServiceWorkerGlobalScope

self.addEventListener('fetch', event => {
    const url = new URL(event.request.url)
    if (url.hostname === 'localhost' && url.pathname !== '/esbuild') {
        event.respondWith(
            fetch(event.request.url).then(response => {
                const clonedResponse = new Response(response.body, response)
                clonedResponse.headers.set('Cross-Origin-Embedder-Policy', 'require-corp')
                clonedResponse.headers.set('Cross-Origin-Opener-Policy', 'same-origin')
                clonedResponse.headers.set('Cross-Origin-Resource-Policy', 'same-origin')
                return clonedResponse
            })
        )
        return
    } else {
        return false
    }
})

// Required for TypeScript to consider it a module, instead of in window scope.
export {}
