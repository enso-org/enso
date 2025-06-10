/** @file Actions for the "members" tab of the "settings" page. */
import BaseSettingsTabActions from './BaseSettingsTabActions'
import { goToSettingsTabActions, type GoToSettingsTabActions } from './gotoSettingsTabActions'

/** Actions for the "members" tab of the "settings" page. */
export default class SettingsMembersTabActions<Context> extends BaseSettingsTabActions<Context> {
  /** Actions for navigating to another settings tab. */
  get goToSettingsTab(): Omit<GoToSettingsTabActions<Context>, 'members'> {
    return goToSettingsTabActions(this.step.bind(this))
  }
}
