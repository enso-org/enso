/** @file A sub-page of the settings page. */

export const SETTINGS_TAB_TYPES = [
  'account',
  'activity-log',
  'billing-and-plans',
  'keyboard-shortcuts',
  'local',
  'members',
  'organization',
  'user-groups',
] as const

/** A sub-page of the settings page. */
export type SettingsTabType = (typeof SETTINGS_TAB_TYPES)[number]

// Settings tabs visible in mockup but without defined features yet.
// 'features' | 'notifications' | 'appearance' | 'data-co-pilot' | 'feature-preview' | 'compliance' | 'usage-statistics' | 'personal-access-token'
