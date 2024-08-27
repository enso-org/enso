/** @file Actions for the third step of the "setup" page. */
import { TEXT } from '../actions'
import BaseActions from './BaseActions'
import SetupDonePageActions from './SetupDonePageActions'

// ============================
// === SetupTeamPageActions ===
// ============================

/** Actions for the third step of the "setup" page. */
export default class SetupTeamPageActions extends BaseActions {
  /** Set the userame for a new user that does not yet have a username. */
  test() {
    return this.step(`fake`, async (page) => {}).into(SetupDonePageActions)
  }
}
