/** @file Actions for the "settings" page. */
import BaseActions from './BaseActions'
import * as goToPageActions from './goToPageActions'
import * as openUserMenuAction from './openUserMenuAction'
import * as userMenuActions from './userMenuActions'

// ===========================
// === SettingsPageActions ===
// ===========================

// TODO: split settings page actions into different classes for each settings tab.
/** Actions for the "settings" page. */
export default class SettingsPageActions extends BaseActions {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<goToPageActions.GoToPageActions, 'settings'> {
    return goToPageActions.goToPageActions(this.step.bind(this))
  }

  /** Actions related to the User Menu. */
  get userMenu() {
    return userMenuActions.userMenuActions(this.step.bind(this))
  }

  /** Open the User Menu. */
  openUserMenu() {
    return openUserMenuAction.openUserMenuAction(this.step.bind(this))
  }
}
