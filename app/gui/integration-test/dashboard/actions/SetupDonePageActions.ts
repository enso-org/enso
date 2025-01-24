/** @file Actions for the fourth step of the "setup" page. */
import { TEXT } from '.'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'

/** Actions for the fourth step of the "setup" page. */
export default class SetupDonePageActions<Context> extends BaseActions<Context> {
  /** Go to the drive page. */
  get goToPage() {
    return {
      drive: () =>
        this.step("Finish setup and go to 'drive' page", async (page) => {
          await page.getByText(TEXT.goToDashboard).click()
        }).into(DrivePageActions<Context>),
    }
  }
}
