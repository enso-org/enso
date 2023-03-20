/** @file This module contains metadata about the product and distribution.
 *
 * Code in this package is used by two or more sibling packages of this package. The code is defined
 * here when it is not possible for a sibling package to own that code without introducing a
 * circular dependency in our packages. */

/** URL protocol scheme for deep links to authentication flow pages.
 *
 * For example: the deep link URL
 * `enso://authentication/register?code=...&state=...` uses this scheme. */
export const DEEP_LINK_SCHEME = 'enso'

/** Name of the product. */
export const PRODUCT_NAME = 'Enso'
