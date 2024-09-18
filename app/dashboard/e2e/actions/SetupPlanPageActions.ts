/** @file Actions for the second step of the "setup" page. */
import { PLAN_TO_UPGRADE_LABEL_ID } from '#/modules/payments/constants'
import { Plan } from 'enso-common/src/services/Backend'
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
  selectSoloPlan() {
    return this.step(`Select 'solo' plan`, async (page) => {
      await page.getByLabel(TEXT[PLAN_TO_UPGRADE_LABEL_ID[Plan.solo]]).click()
      await page
        .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
        .getByText(TEXT.licenseAgreementCheckbox)
        .click()
      await page.getByText(TEXT.startTrial).click()
    }).into(SetupDonePageActions)
  }

  /** Select a plan. */
  selectPlan(plan: Exclude<Plan, Plan.free | Plan.solo>) {
    return this.step(`Select '${plan}' plan`, async (page) => {
      await page.getByLabel(TEXT[PLAN_TO_UPGRADE_LABEL_ID[plan]]).click()
      await page
        .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
        .getByText(TEXT.licenseAgreementCheckbox)
        .click()
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
