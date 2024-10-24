/**
 * @file This module contains metadata about the product and distribution,
 * and various other constants that are needed in multiple sibling packages.
 *
 * Code in this package is used by two or more sibling packages of this package. The code is defined
 * here when it is not possible for a sibling package to own that code without introducing a
 * circular dependency in our packages.
 */

// ========================
// === Product metadata ===
// ========================

/**
 * URL protocol scheme for deep links to authentication flow pages, without the `:` suffix.
 *
 * For example: the deep link URL
 * `enso://authentication/register?code=...&state=...` uses this scheme
 */
export const DEEP_LINK_SCHEME: string = 'enso'

/** Name of the product. */
export const PRODUCT_NAME: string = 'Enso'

/** Company name, used as the copyright holder. */
export const COMPANY_NAME: string = 'New Byte Order sp. z o.o.'

/**
 * The domain on which the Cloud Dashboard web app is hosted.
 * Excludes the protocol (`https://`).
 */
export const CLOUD_DASHBOARD_DOMAIN: string = 'cloud.enso.org'

/**
 * COOP, COEP, and CORP headers: https://web.dev/coop-coep/
 *
 * These are required to increase the resolution of `performance.now()` timers,
 * making profiling a lot more accurate and consistent.
 */
export const COOP_COEP_CORP_HEADERS: [header: string, value: string][] = [
  ['Cross-Origin-Opener-Policy', 'same-origin'],
  ['Cross-Origin-Resource-Policy', 'same-origin'],
]
