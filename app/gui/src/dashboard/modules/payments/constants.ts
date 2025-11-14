/** @file Constants for the subscribe page. */
import { Plan } from 'enso-common/src/services/Backend'

/* eslint-disable @typescript-eslint/no-magic-numbers */

/** The text id for the plan name. */
export const PLAN_TO_TEXT_ID: { readonly [PlanVariant in Plan]: `${PlanVariant}PlanName` } = {
  [Plan.free]: 'freePlanName',
  [Plan.solo]: 'soloPlanName',
  [Plan.team]: 'teamPlanName',
  [Plan.enterprise]: 'enterprisePlanName',
}

/** The text id for the plan name. */
export const PLAN_TO_UPGRADE_LABEL_ID: {
  readonly [PlanVariant in Plan]: `${PlanVariant}PlanUpgradeLabel`
} = {
  [Plan.free]: 'freePlanUpgradeLabel',
  [Plan.solo]: 'soloPlanUpgradeLabel',
  [Plan.team]: 'teamPlanUpgradeLabel',
  [Plan.enterprise]: 'enterprisePlanUpgradeLabel',
}

export const PRICE_CURRENCY = 'USD'

export const PRICE_BY_PLAN: Readonly<Record<Plan, number>> = {
  [Plan.free]: 0,
  [Plan.solo]: 75,
  [Plan.team]: 150,
  [Plan.enterprise]: 250,
}

export const TRIAL_DURATION_DAYS = 30

export const MAX_SEATS_BY_PLAN: Readonly<Record<Plan, number>> = {
  [Plan.free]: 1,
  [Plan.solo]: 1,
  [Plan.team]: 10,
  [Plan.enterprise]: Infinity,
}
