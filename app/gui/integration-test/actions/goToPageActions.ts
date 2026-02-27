/** @file Actions for going to a different page. */
import type { PageCallback } from './BaseActions'
import BaseActions from './BaseActions'
import EditorPageActions from './EditorPageActions'
import SettingsPageActions from './SettingsPageActions'

/** Actions for going to a different page. */
export interface GoToPageActions<Context> {
  readonly projectView: () => EditorPageActions<Context>
  readonly settings: () => SettingsPageActions<Context>
}

/** Generate actions for going to a different page. */
export function goToPageActions<Context>(
  step: (
    name: string,
    callback: PageCallback<Context, BaseActions<Context>>,
  ) => BaseActions<Context>,
): GoToPageActions<Context> {
  return {
    projectView: () =>
      step('Go to Project page', (page) =>
        page.getByTestId('project-view-tab-button').click(),
      ).into(EditorPageActions<Context>),
    settings: () =>
      step('Go to "settings" page', (page) => BaseActions.press(page, 'Mod+,')).into(
        SettingsPageActions<Context>,
      ),
  }
}
