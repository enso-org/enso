/** @file Actions common to all pages. */
import BaseActions from './BaseActions'
import { openUserMenuAction } from './openUserMenuAction'
import { userMenuActions } from './userMenuActions'

/** Actions common to all pages. */
export default class PageActions<Context> extends BaseActions<Context> {
  /** Actions related to the User Menu. */
  get userMenu() {
    return userMenuActions(this.step.bind(this))
  }

  /** Open the User Menu. */
  openUserMenu() {
    return openUserMenuAction(this.step.bind(this))
  }
}
