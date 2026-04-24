/** @file Actions for the "user groups" tab of the "settings" page. */
import BaseSettingsTabActions from './BaseSettingsTabActions'
import { goToSettingsTabActions, type GoToSettingsTabActions } from './gotoSettingsTabActions'

/** Actions for the "user groups" tab of the "settings" page. */
export default class SettingsUserGroupsTabActions<Context> extends BaseSettingsTabActions<Context> {
  /** Actions for navigating to another settings tab. */
  get goToSettingsTab(): Omit<GoToSettingsTabActions<Context>, 'userGroups'> {
    return goToSettingsTabActions(this.step.bind(this))
  }
}
