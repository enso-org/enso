/** @file Actions for the "setup" page. */
import { TEXT } from '../actions'
import BaseActions from './BaseActions'
import SetupPlanPageActions from './SetupPlanPageActions'

// ================================
// === SetupUsernamePageActions ===
// ================================

/** Actions for the "setup" page. */
export default class SetupUsernamePageActions extends BaseActions {
  /** Set the userame for a new user that does not yet have a username. */
  setUsername(username: string) {
    return this.step(`Set username to '${username}'`, async (page) => {
      await page.getByPlaceholder('Enter your username').fill(username)
      await page.getByText(TEXT.next).click()
    }).into(SetupPlanPageActions)
  }
}
