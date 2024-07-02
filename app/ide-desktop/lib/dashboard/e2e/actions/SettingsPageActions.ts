/** @file Actions for the "settings" page. */
import * as goToPageActions from './goToPageActions'
import PageActions from './PageActions'

// ===========================
// === SettingsPageActions ===
// ===========================

// TODO: split settings page actions into different classes for each settings tab.
/** Actions for the "settings" page. */
export default class SettingsPageActions extends PageActions {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<goToPageActions.GoToPageActions, 'drive'> {
    return goToPageActions.goToPageActions(this.step.bind(this))
  }
}
