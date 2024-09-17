/** @file Actions for the "setup" page. */
import * as actions from '../actions'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'

// ==============================
// === SetUsernamePageActions ===
// ==============================

/** Actions for the "set username" page. */
export default class SetupPageActions extends BaseActions {
  /** Set the userame for a new user that does not yet have a username. */
  setUsername(username: string) {
    return this.step(`Set username to '${username}'`, async (page) => {
      await actions.locateUsernameInput(page).fill(username)
      await actions.locateSetUsernameButton(page).click()
    }).into(DrivePageActions)
  }
}
