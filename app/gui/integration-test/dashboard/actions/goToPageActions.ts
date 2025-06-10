/** @file Actions for going to a different page. */
import type { PageCallback } from './BaseActions'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'
import EditorPageActions from './EditorPageActions'
import SettingsPageActions from './SettingsPageActions'

/** Actions for going to a different page. */
export interface GoToPageActions<Context> {
  readonly drive: () => DrivePageActions<Context>
  readonly editor: () => EditorPageActions<Context>
  readonly settings: () => SettingsPageActions<Context>
}

/** Generate actions for going to a different page. */
export function goToPageActions<Context>(
  step: (name: string, callback: PageCallback<Context>) => BaseActions<Context>,
): GoToPageActions<Context> {
  return {
    drive: () =>
      step('Go to "Data Catalog" page', (page) =>
        page
          .getByRole('tab')
          .filter({ has: page.getByText('Data Catalog') })
          .click(),
      ).into(DrivePageActions<Context>),
    editor: () =>
      step('Go to "Spatial Analysis" page', (page) =>
        page.getByTestId('editor-tab-button').click(),
      ).into(EditorPageActions<Context>),
    settings: () =>
      step('Go to "settings" page', (page) => BaseActions.press(page, 'Mod+,')).into(
        SettingsPageActions<Context>,
      ),
  }
}
