/** @file Actions for the "home" page. */
import * as actions from '.'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'
import EditorPageActions from './EditorPageActions'

// =========================
// === StartModalActions ===
// =========================

/** Actions for the "start" modal. */
export default class StartModalActions extends BaseActions {
  /** Close this modal and go back to the Drive page. */
  close() {
    return this.step('Close "start" modal', (page) => page.getByLabel('Close').click()).into(
      DrivePageActions,
    )
  }

  /** Create a project from the template at the given index. */
  createProjectFromTemplate(index: number) {
    return this.step(`Create project from template #${index}`, (page) =>
      actions
        .locateSamples(page)
        .nth(index + 1)
        .click(),
    ).into(EditorPageActions)
  }
}
