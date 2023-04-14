/** @file A service worker that redirects paths without extensions to `/index.html`. */
/// <reference lib="WebWorker" />

// =================
// === Constants ===
// =================

const IDE_CDN_URL = 'https://ensocdn.s3.us-west-1.amazonaws.com/ide'
const FALLBACK_VERSION = '2023.1.1-nightly.2023.4.13'

// =====================
// === Fetch handler ===
// =====================

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
    } else if (url.hostname === 'localhost' && url.pathname === '/style.css') {
        event.respondWith(fetch(`${IDE_CDN_URL}/${FALLBACK_VERSION}/style.css`))
        return
    } else {
        return false
    }
})

// Required for TypeScript to consider it a module, instead of in window scope.
export {}
