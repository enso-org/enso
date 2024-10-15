/** @file Actions for the fourth step of the "setup" page. */
import { TEXT } from '.'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'

// ============================
// === SetupDonePageActions ===
// ============================

/** Actions for the fourth step of the "setup" page. */
export default class SetupDonePageActions extends BaseActions {
  /** Go to the drive page. */
  get goToPage() {
    return {
      drive: () =>
        this.step("Finish setup and go to 'drive' page", async (page) => {
          await page.getByText(TEXT.goToDashboard).click()
        }).into(DrivePageActions),
    }
  }
}
