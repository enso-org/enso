/** @file Actions for the "local" tab of the "settings" page. */
import BaseSettingsTabActions from './BaseSettingsTabActions'
import { goToSettingsTabActions, type GoToSettingsTabActions } from './gotoSettingsTabActions'

/** Actions for the "local" tab of the "settings" page. */
export default class SettingsLocalTabActions<Context> extends BaseSettingsTabActions<Context> {
  /** Actions for navigating to another settings tab. */
  get goToSettingsTab(): Omit<GoToSettingsTabActions<Context>, 'local'> {
    return goToSettingsTabActions(this.step.bind(this))
  }
}
