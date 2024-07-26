/** @file Actions for going to a different page. */
import type * as baseActions from './BaseActions'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'
import EditorPageActions from './EditorPageActions'
import SettingsPageActions from './SettingsPageActions'

// =======================
// === GoToPageActions ===
// =======================

/** Actions for going to a different page. */
export interface GoToPageActions {
  readonly drive: () => DrivePageActions
  readonly editor: () => EditorPageActions
  readonly settings: () => SettingsPageActions
}

// =======================
// === goToPageActions ===
// =======================

/** Generate actions for going to a different page. */
export function goToPageActions(
  step: (name: string, callback: baseActions.PageCallback) => BaseActions,
): GoToPageActions {
  return {
    drive: () =>
      step('Go to "Data Catalog" page', (page) =>
        page
          .getByRole('tab')
          .filter({ has: page.getByText('Data Catalog') })
          .click(),
      ).into(DrivePageActions),
    editor: () =>
      step('Go to "Spatial Analysis" page', (page) =>
        page.getByTestId('editor-tab-button').click(),
      ).into(EditorPageActions),
    settings: () =>
      step('Go to "settings" page', (page) => BaseActions.press(page, 'Mod+,')).into(
        SettingsPageActions,
      ),
  }
}
