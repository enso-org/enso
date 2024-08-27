/** @file Actions for the second step of the "setup" page. */
import { PLAN_TO_UPGRADE_LABEL_ID } from '#/modules/payments/constants'
import type { Plan } from 'enso-common/src/services/Backend'
import { TEXT } from '../actions'
import BaseActions from './BaseActions'
import SetupDonePageActions from './SetupDonePageActions'
import SetupTeamPageActions from './SetupTeamPageActions'

// ============================
// === SetupPlanPageActions ===
// ============================

/** Actions for the second step of the "setup" page. */
export default class SetupPlanPageActions extends BaseActions {
  /** Select a plan. */
  selectPlan(plan: Exclude<Plan, Plan.free>) {
    return this.step(`Select '${plan}' plan`, async (page) => {
      await page.getByLabel(TEXT[PLAN_TO_UPGRADE_LABEL_ID[plan]]).click()
      await page.getByText(TEXT.startTrial).click()
    }).into(SetupTeamPageActions)
  }

  /** Stay on the current (free) plan. */
  stayOnFreePlan() {
    return this.step(`Stay on current plan`, async (page) => {
      await page.getByText(TEXT.skip).click()
    }).into(SetupDonePageActions)
  }

  /** Stay on the current (paid) plan. */
  stayOnPaidPlan() {
    return this.step(`Stay on current plan`, async (page) => {
      await page.getByText(TEXT.skip).click()
    }).into(SetupTeamPageActions)
  }
}
