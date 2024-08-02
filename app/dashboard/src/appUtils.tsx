/** @file Constants related to the application root component. */
import type { DriveCategory } from '#/layouts/CategorySwitcher/Category'
import type { SettingsTabType } from '#/pages/dashboard/Settings/settingsData'

// =================
// === Constants ===
// =================

export const SEARCH_PARAMS_PREFIX = 'cloud-ide_'

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
  'setup',
  'subscribe',
  'subscribe/success',
] as const

/** Valid paths for a page in the app. */
export type AppPath =
  | Exclude<(typeof APP_PATHS)[number], 'editor' | 'settings'>
  | `drive/${DriveCategory}`
  | `editor/${string}`
  | `settings/${SettingsTabType}`

/** Valid paths to a page in the app, including the leading slash and the search query
 * (if any). */
export type AppFullPath = `/${AppPath}${'' | `?${string}`}`

/** A type-safe function to create a path. */
export function definePath(path: AppPath) {
  return path
}

/** A {@link RegExp} matching all paths. */
export const ALL_PATHS_REGEX = new RegExp(
  `/$|/(?:${APP_PATHS.map((path) => (path === 'editor' ? `${path}/` : path)).join('|')})\\b.*?$`,
)

/**
 * Build a Subscription URL for a given plan.
 */
export function getUpgradeURL(plan: string): AppFullPath {
  return `/subscribe?plan=${plan}`
}

/**
 * Return the mailto URL for contacting sales.
 */
export function getSalesEmail(): string {
  return 'mailto:contact@enso.org'
}

/**
 * Build a Subscription URL for contacting sales.
 */
export function getContactSalesURL(): `mailto:${string}` {
  return 'mailto:contact@enso.org?subject=Upgrading%20to%20Organization%20Plan'
}

/** A type predicate for whether a path is a valid app path. */
export function isAppFullPath(path: string): path is AppFullPath {
  return ALL_PATHS_REGEX.test(path)
}
