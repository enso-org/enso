/** @file Actions for going to a different page. */
import type * as baseActions from './BaseActions'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'
import EditorPageActions from './EditorPageActions'
import HomePageActions from './HomePageActions'
import SettingsPageActions from './SettingsPageActions'

// =======================
// === GoToPageActions ===
// =======================

/** Actions for going to a different page. */
export interface GoToPageActions {
  readonly home: () => HomePageActions
  readonly drive: () => DrivePageActions
  readonly editor: () => EditorPageActions
  readonly settings: () => SettingsPageActions
}

// =======================
// === goToPageActions ===
// =======================

/** Generate actions for going to a different page. */
export function goToPageActions(
  step: (name: string, callback: baseActions.PageCallback) => BaseActions
): GoToPageActions {
  return {
    home: () =>
      step('Go to "Home" page', page =>
        page
          .getByRole('button')
          .filter({ has: page.getByAltText('Home') })
          .click()
      ).into(HomePageActions),
    drive: () =>
      step('Go to "Drive" page', page =>
        page
          .getByRole('button')
          .filter({ has: page.getByAltText('Catalog') })
          .click()
      ).into(DrivePageActions),
    editor: () =>
      step('Go to "Graph Editor" page', page =>
        page
          .getByRole('button')
          .filter({ has: page.getByAltText('Graph Editor') })
          .click()
      ).into(EditorPageActions),
    settings: () =>
      step('Go to "settings" page', page => BaseActions.press(page, 'Mod+,')).into(
        SettingsPageActions
      ),
  }
}
