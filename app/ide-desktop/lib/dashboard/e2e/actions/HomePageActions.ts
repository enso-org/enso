/** @file Actions for the "home" page. */
import * as actions from '../actions'
import BaseActions from './BaseActions'
import EditorPageActions from './EditorPageActions'
import * as goToPageActions from './goToPageActions'
import * as openUserMenuAction from './openUserMenuAction'
import * as userMenuActions from './userMenuActions'

// =======================
// === HomePageActions ===
// =======================

/** Actions for the "home" page. */
export default class HomePageActions extends BaseActions {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<goToPageActions.GoToPageActions, 'home'> {
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

  /** Create an empty project. */
  createEmptyProject() {
    return this.step('Create empty project', page =>
      actions.locateSamples(page).nth(0).click()
    ).into(EditorPageActions)
  }

  /** Create a project from the template at the given index. */
  createProjectFromTemplate(index: number) {
    return this.step(`Create project from template #${index}`, page =>
      actions
        .locateSamples(page)
        .nth(index + 1)
        .click()
    ).into(EditorPageActions)
  }
}
