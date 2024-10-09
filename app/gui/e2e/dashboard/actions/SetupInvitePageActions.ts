/** @file Actions for the third step of the "setup" page. */
import { TEXT } from '.'
import BaseActions from './BaseActions'
import SetupTeamPageActions from './SetupTeamPageActions'

// ==============================
// === SetupInvitePageActions ===
// ==============================

/** Actions for the "invite users" step of the "setup" page. */
export default class SetupInvitePageActions extends BaseActions {
  /** Invite users by email. */
  inviteUsers(emails: string) {
    return this.step(`Invite users '${emails.split(/[ ;,]+/).join("', '")}'`, async (page) => {
      await page.getByLabel(TEXT.inviteEmailFieldLabel).getByRole('textbox').fill(emails)
      await page.getByText(TEXT.inviteSubmit).click()
    }).into(SetupTeamPageActions)
  }

  /** Continue to the next step without inviting users. */
  skipInvitingUsers() {
    return this.step('Skip inviting users in setup', async (page) => {
      await page.getByText(TEXT.skip).click()
    }).into(SetupTeamPageActions)
  }
}
