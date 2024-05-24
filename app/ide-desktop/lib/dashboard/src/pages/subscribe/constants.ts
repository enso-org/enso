/**
 * @file Constants for the subscribe page.
 */
import type * as text from '#/text'

import * as backendModule from '#/services/Backend'

/**
 * The text id for the plan name.
 */
export const PLAN_TO_TEXT_ID: Readonly<Record<backendModule.Plan, text.TextId>> = {
  [backendModule.Plan.solo]: 'soloPlanName',
  [backendModule.Plan.team]: 'teamPlanName',
  [backendModule.Plan.enterprise]: 'enterprisePlanName',
} satisfies { [Plan in backendModule.Plan]: `${Plan}PlanName` }
