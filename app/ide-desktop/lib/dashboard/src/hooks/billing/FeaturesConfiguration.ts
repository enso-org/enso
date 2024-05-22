/**
 * @file
 *
 * Paywall configuration for different plans.
 */

import type * as text from '#/text'

import * as backend from '#/services/Backend'

/**
 * Registered paywall features.
 */
export const PAYWALL_FEATURES = {
  userGroups: 'userGroups',
  userGroupsFull: 'userGroupsFull',
} as const

/**
 * Paywall features.
 */
export type PaywallFeatureName = keyof typeof PAYWALL_FEATURES

/**
 * Paywall level names
 */
export type PaywallLevelName = backend.Plan | 'free'

/**
 * Paywall level values.
 * Used to define the paywall levels and their corresponding labels.
 * The value is a number that represents the level of the paywall.
 * Because the paywall levels are ordered and inclusive, the value is used to compare the levels.
 */
export type PaywallLevelValue =
  | (0 & { readonly name: PaywallLevelName; readonly label: text.TextId })
  | (1 & { readonly name: PaywallLevelName; readonly label: text.TextId })
  | (2 & { readonly name: PaywallLevelName; readonly label: text.TextId })
  | (3 & { readonly name: PaywallLevelName; readonly label: text.TextId })

/**
 * Paywall levels configuration.
 */
export const PAYWALL_LEVELS: Record<PaywallLevelName, PaywallLevelValue> = {
  free: Object.assign(0, { name: 'free', label: 'freePlanName' } as const),
  [backend.Plan.solo]: Object.assign(1, {
    name: backend.Plan.solo,
    label: 'soloPlanName',
  } as const),
  [backend.Plan.team]: Object.assign(2, {
    name: backend.Plan.team,
    label: 'teamPlanName',
  } as const),
  [backend.Plan.enterprise]: Object.assign(3, {
    name: backend.Plan.enterprise,
    label: 'enterprisePlanName',
  } as const),
}

/**
 *
 */
export type PaywallLevel = (typeof PAYWALL_LEVELS)[keyof typeof PAYWALL_LEVELS]

/**
 * Paywall feature labels.
 */
const PAYWALL_FEATURES_LABELS: Record<PaywallFeatureName, text.TextId> = {
  userGroups: 'userGroupsFeatureLabel',
  userGroupsFull: 'userGroupsFullFeatureLabel',
} satisfies { [K in PaywallFeatureName]: `${K}FeatureLabel` }

/**
 * Basic feature configuration.
 */
interface BasicFeatureConfiguration {
  readonly level: PaywallLevel
  readonly bulletPointsTextId: `${PaywallFeatureName}FeatureBulletPoints`
  readonly descriptionTextId: `${PaywallFeatureName}FeatureDescription`
}

/**
 * Feature configuration.
 */
export type FeatureConfiguration<Key extends PaywallFeatureName = PaywallFeatureName> =
  BasicFeatureConfiguration & {
    readonly name: Key
    readonly label: (typeof PAYWALL_FEATURES_LABELS)[Key]
  }

const PAYWALL_CONFIGURATION: Record<PaywallFeatureName, BasicFeatureConfiguration> = {
  userGroups: {
    level: PAYWALL_LEVELS.team,
    bulletPointsTextId: 'userGroupsFeatureBulletPoints',
    descriptionTextId: 'userGroupsFeatureDescription',
  },
  userGroupsFull: {
    level: PAYWALL_LEVELS.enterprise,
    bulletPointsTextId: 'userGroupsFullFeatureBulletPoints',
    descriptionTextId: 'userGroupsFullFeatureDescription',
  },
}

/**
 * Map a plan to a paywall level.
 */
export function mapPlanOnPaywall(plan: backend.Plan | undefined): PaywallLevel {
  return plan != null ? PAYWALL_LEVELS[plan] : PAYWALL_LEVELS.free
}

/**
 * Check if a given string is a valid feature name.
 */
export function isFeatureName(name: string): name is PaywallFeatureName {
  return name in PAYWALL_FEATURES
}

/**
 * Get the configuration for a given feature.
 */
export function getFeatureConfiguration(feature: PaywallFeatureName): FeatureConfiguration {
  const configuration = PAYWALL_CONFIGURATION[feature]

  return {
    ...configuration,
    name: feature,
    label: PAYWALL_FEATURES_LABELS[feature],
  }
}
