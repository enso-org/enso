/** @file Actions for the second step of the "setup" page. */
import { PLAN_TO_TEXT_ID } from '#/modules/payments/constants'
import type { Plan } from 'enso-common/src/services/Backend'
import { TEXT } from '../actions'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'
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
      await page.getByText(PLAN_TO_TEXT_ID[plan]).click()
    }).into(DrivePageActions)
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
