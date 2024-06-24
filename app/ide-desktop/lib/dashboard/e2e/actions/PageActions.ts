/** @file Actions common to all pages. */

import BaseActions from './BaseActions'
import * as goToPageActions from './goToPageActions'
import * as openUserMenuAction from './openUserMenuAction'
import * as userMenuActions from './userMenuActions'

// ===================
// === PageActions ===
// ===================

/** Actions common to all pages. */
export default class PageActions<
  Page extends keyof goToPageActions.GoToPageActions,
> extends BaseActions {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<goToPageActions.GoToPageActions, Page> {
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
