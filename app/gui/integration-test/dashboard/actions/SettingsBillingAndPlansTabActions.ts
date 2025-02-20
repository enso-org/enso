/** @file Actions for the "billing and plans" tab of the "settings" page. */
import BaseSettingsTabActions from './BaseSettingsTabActions'
import { goToSettingsTabActions, type GoToSettingsTabActions } from './gotoSettingsTabActions'

/** Actions for the "billing and plans" tab of the "settings" page. */
export default class SettingsBillingAndPlansTabActions<
  Context,
> extends BaseSettingsTabActions<Context> {
  /** Actions for navigating to another settings tab. */
  get goToSettingsTab(): Omit<GoToSettingsTabActions<Context>, 'billingAndPlans'> {
    return goToSettingsTabActions(this.step.bind(this))
  }
}
