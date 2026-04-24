/** @file Actions for the "keyboard shortcuts" tab of the "settings" page. */
import BaseSettingsTabActions from './BaseSettingsTabActions'
import { goToSettingsTabActions, type GoToSettingsTabActions } from './gotoSettingsTabActions'

/** Actions for the "keyboard shortcuts" tab of the "settings" page. */
export default class SettingsKeyboardShortcutsTabActions<
  Context,
> extends BaseSettingsTabActions<Context> {
  /** Actions for navigating to another settings tab. */
  get goToSettingsTab(): Omit<GoToSettingsTabActions<Context>, 'keyboardShortcuts'> {
    return goToSettingsTabActions(this.step.bind(this))
  }
}
