/** @file Actions for the "editor" page. */
import { goToPageActions, type GoToPageActions } from './goToPageActions'
import PageActions from './PageActions'

/** Actions for the "editor" page. */
export default class EditorPageActions<Context> extends PageActions<Context> {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<GoToPageActions<Context>, 'editor'> {
    return goToPageActions(this.step.bind(this))
  }

  /** Wait for the editor to load. */
  waitForEditorToLoad(): EditorPageActions<Context> {
    return this.step('Wait for the editor to load', async () => {
      await this.page.waitForSelector('[data-testid=editor]', { state: 'visible' })
    })
  }

  /** Close all toast notifications. */
  closeToastNotifications() {
    return this.step('Close toast notifications', async () => {
      await Promise.all(
        await this.page
          .locator('.Toastify__toast')
          .getByRole('button')
          .all()
          .then((buttons) => buttons.map((button) => button.click())),
      )
    })
  }
}
