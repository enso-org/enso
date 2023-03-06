/** @file This module defines resources shared between the:
 * - code building the client package, and
 * - the packaged code itself.
 *
 * The shared resources are:
 * - paths within the client distribution's resources,
 * - the name of the product, and
 * - custom URL protocol scheme definitions. */

/** Path to the Project Manager bundle. */
export const PROJECT_MANAGER_BUNDLE = 'enso'

/** Name of the product. */
export const PRODUCT_NAME = 'Enso'

/** URL protocol scheme for deep links to authentication flow pages. */
export const DEEP_LINK_SCHEME = 'enso'
