/** @file This module contains metadata about the product and distribution.
 *
 * The code in this module was originally used by both the `enso` and the
 * `enso-authentication` packages. The `enso` package depends on the
 * `enso-authentication` package. To avoid a circular dependency, the common code was moved
 * here. */

/** URL protocol scheme for deep links to authentication flow pages.
 *
 * For example: the deep link URL
 * `enso://authentication/register?code=...&state=...` uses this scheme. */
export const DEEP_LINK_SCHEME = 'enso'
