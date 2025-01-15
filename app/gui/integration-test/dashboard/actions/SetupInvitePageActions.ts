/** @file Actions for the third step of the "setup" page. */
import { TEXT } from '.'
import BaseActions from './BaseActions'
import SetupTeamPageActions from './SetupTeamPageActions'

/** Actions for the "invite users" step of the "setup" page. */
export default class SetupInvitePageActions<Context> extends BaseActions<Context> {
  /** Invite users by email. */
  inviteUsers(emails: string) {
    return this.step(`Invite users '${emails.split(/[ ;,]+/).join("', '")}'`, async (page) => {
      await page.getByLabel(TEXT.inviteEmailFieldLabel).getByRole('textbox').fill(emails)
      await page.getByText(TEXT.inviteSubmit).click()
    }).into(SetupTeamPageActions<Context>)
  }

  /** Continue to the next step without inviting users. */
  skipInvitingUsers() {
    return this.step('Skip inviting users in setup', async (page) => {
      await page.getByText(TEXT.skip).click()
    }).into(SetupTeamPageActions<Context>)
  }
}
