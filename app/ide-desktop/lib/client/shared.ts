/** @file This module contains metadata about the product and distribution.
 * For example, it contains:
 * - the name of the product, and
 * - custom URL protocol scheme definitions.
 *
 * This metadata is used in both the code building the client resources and the packaged code
 * itself. */

/** Name of the product. */
export const PRODUCT_NAME = 'Enso'

/** URL protocol scheme for deep links to authentication flow pages.
 *
 * For example: the deep link URL
 * `enso://authentication/register?code=...&state=...` uses this scheme. */
export const DEEP_LINK_SCHEME = 'enso'
