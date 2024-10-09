/** @file Actions for the "setup" page. */
import { TEXT } from '.'
import BaseActions from './BaseActions'
import SetupDonePageActions from './SetupDonePageActions'

// ================================
// === SetupTeamNamePageActions ===
// ================================

/** Actions for the "setup team name" page. */
export default class SetupTeamNamePagePageActions extends BaseActions {
  /** Set the username for a new user that does not yet have a username. */
  setTeamName(teamName: string) {
    return this.step(`Set team name to '${teamName}'`, async (page) => {
      await page
        .getByLabel(TEXT.groupNameSettingsInput)
        .and(page.getByRole('textbox'))
        .fill(teamName)
      await page.getByText(TEXT.next).click()
    }).into(SetupDonePageActions)
  }
}
