/** @file Actions for going to a different page. */
import type * as baseActions from './BaseActions'
import type BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'
import EditorPageActions from './EditorPageActions'

// =======================
// === GoToPageActions ===
// =======================

/** Actions for going to a different page. */
export interface GoToPageActions {
  readonly drive: () => DrivePageActions
  readonly editor: () => EditorPageActions
}

// =======================
// === goToPageActions ===
// =======================

/** Generate actions for going to a different page. */
export function goToPageActions(
  step: (name: string, callback: baseActions.PageCallback) => BaseActions
): GoToPageActions {
  return {
    drive: () =>
      step('Go to "Data Catalog" page', page =>
        page
          .getByRole('button')
          .filter({ has: page.getByText('Data Catalog') })
          .click()
      ).into(DrivePageActions),
    editor: () =>
      step('Go to "Spatial Analysis" page', page =>
        page
          .getByRole('button')
          .filter({ has: page.getByText('Spatial Analysis') })
          .click()
      ).into(EditorPageActions),
  }
}
