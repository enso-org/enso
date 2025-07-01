/** @file Constants for the subscribe page. */
import type * as text from 'enso-common/src/text'

import * as backendModule from '#/services/Backend'

/* eslint-disable @typescript-eslint/no-magic-numbers */

/** The text id for the plan name. */
export const PLAN_TO_TEXT_ID: Readonly<Record<backendModule.Plan, text.TextId>> = {
  [backendModule.Plan.free]: 'freePlanName',
  [backendModule.Plan.solo]: 'soloPlanName',
  [backendModule.Plan.team]: 'teamPlanName',
  [backendModule.Plan.enterprise]: 'enterprisePlanName',
} satisfies { [Plan in backendModule.Plan]: `${Plan}PlanName` }
/** The text id for the plan name. */
export const PLAN_TO_UPGRADE_LABEL_ID: Readonly<Record<backendModule.Plan, text.TextId>> = {
  [backendModule.Plan.free]: 'freePlanUpgradeLabel',
  [backendModule.Plan.solo]: 'soloPlanUpgradeLabel',
  [backendModule.Plan.team]: 'teamPlanUpgradeLabel',
  [backendModule.Plan.enterprise]: 'enterprisePlanUpgradeLabel',
} satisfies { [Plan in backendModule.Plan]: `${Plan}PlanUpgradeLabel` }

export const PRICE_CURRENCY = 'USD'
export const PRICE_BY_PLAN: Readonly<Record<backendModule.Plan, number>> = {
  [backendModule.Plan.free]: 0,
  [backendModule.Plan.solo]: 75,
  [backendModule.Plan.team]: 150,
  [backendModule.Plan.enterprise]: 250,
} satisfies { [Plan in backendModule.Plan]: number }

export const TRIAL_DURATION_DAYS = 30

const TEAM_PLAN_MAX_SEATS = 10
export const MAX_SEATS_BY_PLAN: Record<backendModule.Plan, number> = {
  [backendModule.Plan.enterprise]: Infinity,
  [backendModule.Plan.team]: TEAM_PLAN_MAX_SEATS,
  [backendModule.Plan.solo]: 1,
  [backendModule.Plan.free]: 1,
}
