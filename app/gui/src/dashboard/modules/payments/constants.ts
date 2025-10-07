/** @file Constants for the subscribe page. */
import type { Plan } from '#/services/Backend'

/* eslint-disable @typescript-eslint/no-magic-numbers */

/** The text id for the plan name. */
export const PLAN_TO_TEXT_ID: { readonly [PlanVariant in Plan]: `${PlanVariant}PlanName` } = {
  free: 'freePlanName',
  solo: 'soloPlanName',
  team: 'teamPlanName',
  enterprise: 'enterprisePlanName',
}

/** The text id for the plan name. */
export const PLAN_TO_UPGRADE_LABEL_ID: {
  readonly [PlanVariant in Plan]: `${PlanVariant}PlanUpgradeLabel`
} = {
  free: 'freePlanUpgradeLabel',
  solo: 'soloPlanUpgradeLabel',
  team: 'teamPlanUpgradeLabel',
  enterprise: 'enterprisePlanUpgradeLabel',
}

export const PRICE_CURRENCY = 'USD'

export const PRICE_BY_PLAN: Readonly<Record<Plan, number>> = {
  free: 0,
  solo: 75,
  team: 150,
  enterprise: 250,
}

export const TRIAL_DURATION_DAYS = 30

export const MAX_SEATS_BY_PLAN: Readonly<Record<Plan, number>> = {
  free: 1,
  solo: 1,
  team: 10,
  enterprise: Infinity,
}
