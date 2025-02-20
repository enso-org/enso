/** @file A sub-page of the settings page. */

// =======================
// === SettingsTabType ===
// =======================

/** A sub-page of the settings page. */
enum SettingsTabType {
  account = 'account',
  organization = 'organization',
  local = 'local',
  // features = 'features',
  // notifications = 'notifications',
  billingAndPlans = 'billing-and-plans',
  members = 'members',
  userGroups = 'user-groups',
  // appearance = 'appearance',
  keyboardShortcuts = 'keyboard-shortcuts',
  // dataCoPilot = 'data-co-pilot',
  // featurePreview = 'feature-preview',
  activityLog = 'activity-log',
  // compliance = 'compliance',
  // usageStatistics = 'usage-statistics',
  // personalAccessToken = 'personal-access-token',
}

export default SettingsTabType
