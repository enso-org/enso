/** @file Actions for the third step of the "setup" page. */
import { TEXT } from '../actions'
import BaseActions from './BaseActions'
import SetupTeamPageActions from './SetupTeamPageActions'

// ==============================
// === SetupInvitePageActions ===
// ==============================

/** Actions for the "invite users" step of the "setup" page. */
export default class SetupInvitePageActions extends BaseActions {
  /** Invite users by email. */
  inviteUsers(emails: string) {
    return this.step(`Invite users '${emails}'`, async (page) => {
      await page
        .getByLabel(TEXT.organizationNameSettingsInput)
        .and(page.getByRole('textbox'))
        .fill(emails)
      await page.getByText(TEXT.next).click()
    }).into(SetupTeamPageActions)
  }

  /** Continue to the next step without inviting users. */
  skip() {
    return this.step('Skip inviting users in setup', async (page) => {
      await page.getByText(TEXT.skip).click()
    }).into(SetupTeamPageActions)
  }
}
