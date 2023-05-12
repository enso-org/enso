/** @file This module contains metadata about the product and distribution,
 * and various other constants that are needed in multiple sibling packages.
 *
 * Code in this package is used by two or more sibling packages of this package. The code is defined
 * here when it is not possible for a sibling package to own that code without introducing a
 * circular dependency in our packages. */

// ================
// === Platform ===
// ================

/** Defines the platform the application is running on.
 *
 * Depending on the platform, the application will use different routing mechanisms. For example, in
 * the cloud, the application will use the browser's URL bar to navigate between pages. In Electron,
 * the application will use the `MemoryRouter` to navigate between pages. Similarly, the
 * application will use different redirect URLs for authentication. For example, in the cloud, the
 * application will redirect using `http://` URLs. In Electron, the application will redirect using
 * `enso://` URLs. The former flow works entirely in-browser. The latter flow must go to the browser
 * and back to the application. */
export enum Platform {
    /** Application is running on a desktop (i.e., in Electron). */
    desktop = 'desktop',
    /** Application is running in the browser (i.e., in the cloud). */
    cloud = 'cloud',
}

/** URL protocol scheme for deep links to authentication flow pages, without the `:` suffix.
 *
 * For example: the deep link URL
 * `enso://authentication/register?code=...&state=...` uses this scheme. */
export const DEEP_LINK_SCHEME = 'enso'

/** Name of the product. */
export const PRODUCT_NAME = 'Enso'

/** COOP, COEP, and CORP headers: https://web.dev/coop-coep/
 *
 * These are required to increase the resolution of `performance.now()` timers,
 * making profiling a lot more accurate and consistent. */
export const COOP_COEP_CORP_HEADERS: [header: string, value: string][] = [
    ['Cross-Origin-Embedder-Policy', 'require-corp'],
    ['Cross-Origin-Opener-Policy', 'same-origin'],
    ['Cross-Origin-Resource-Policy', 'same-origin'],
]
