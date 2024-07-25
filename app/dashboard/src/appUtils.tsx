/** @file Constants related to the application root component. */

// =================
// === Constants ===
// =================

const APP_PATHS = [
  'drive',
  'editor',
  'settings',
  'login',
  'registration',
  'confirmation',
  'restore-user',
  'forgot-password',
  'password-reset',
  'set-username',
  'subscribe',
  'subscribe/success',
] as const

/** Valid paths for a page in the app. */
export type AppPath = (typeof APP_PATHS)[number]

/** Valid paths to a page in the app, including the leading slash and the search query
 * (if any). */
export type AppFullPath = `/${AppPath}${'' | `?${string}`}`

/** A type-safe function to create a path. */
export function definePath(path: AppPath) {
  return path
}

// === Paths ===

/** A {@link RegExp} matching all paths. */
export const ALL_PATHS_REGEX = new RegExp(`\\b(?:${APP_PATHS.join('|')})\\b`)

// === Constants related to URLs ===

export const SEARCH_PARAMS_PREFIX = 'cloud-ide_'

/**
 * Build a Subscription URL for a given plan.
 */
export function getUpgradeURL(plan: string): AppFullPath {
  return `/subscribe?plan=${plan}`
}

/**
 * Build a Subscription URL for contacting sales.
 */
export function getContactSalesURL(): string {
  return 'mailto:contact@enso.org?subject=Upgrading%20to%20Organization%20Plan'
}
