/** @file Actions for the "activity log" tab of the "settings" page. */
import BaseSettingsTabActions from './BaseSettingsTabActions'
import { goToSettingsTabActions, type GoToSettingsTabActions } from './gotoSettingsTabActions'

/** Actions for the "activity log" tab of the "settings" page. */
export default class SettingsActivityLogShortcutsTabActions<
  Context,
> extends BaseSettingsTabActions<Context> {
  /** Actions for navigating to another settings tab. */
  get goToSettingsTab(): Omit<GoToSettingsTabActions<Context>, 'activityLog'> {
    return goToSettingsTabActions(this.step.bind(this))
  }
}
