/** @file Actions for the "user" tab of the "settings" page. */
import { goToPageActions, type GoToPageActions } from './goToPageActions'
import PageActions from './PageActions'

/** Actions common to all settings pages. */
export default class BaseSettingsTabActions<Context> extends PageActions<Context> {
  /** Actions for navigating to another page. */
  get goToPage(): Omit<GoToPageActions<Context>, 'settings'> {
    return goToPageActions(this.step.bind(this))
  }
}
