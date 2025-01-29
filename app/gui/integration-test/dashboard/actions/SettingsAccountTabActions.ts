/** @file Actions for the "account" tab of the "settings" page. */
import BaseSettingsTabActions from './BaseSettingsTabActions'
import SettingsAccountFormActions from './SettingsAccountFormActions'
import SettingsChangePasswordFormActions from './SettingsChangePasswordFormActions'
import { goToSettingsTabActions, type GoToSettingsTabActions } from './gotoSettingsTabActions'

/** Actions for the "account" tab of the "settings" page. */
export default class SettingsAccountTabActions<Context> extends BaseSettingsTabActions<Context> {
  /** Actions for navigating to another settings tab. */
  get goToSettingsTab(): Omit<GoToSettingsTabActions<Context>, 'account'> {
    return goToSettingsTabActions(this.step.bind(this))
  }

  /** Manipulate the "account" form. */
  accountForm() {
    return this.into(SettingsAccountFormActions<Context>)
  }

  /** Manipulate the "change password" form. */
  changePasswordForm() {
    return this.into(SettingsChangePasswordFormActions<Context>)
  }

  /** Upload a profile picture. */
  uploadProfilePicture(
    name: string,
    content: WithImplicitCoercion<string | Uint8Array | readonly number[]>,
    mimeType: string,
  ) {
    return this.step('Upload account profile picture', async (page) => {
      const fileChooserPromise = page.waitForEvent('filechooser')
      await page.getByTestId('user-profile-picture-input').click()
      const fileChooser = await fileChooserPromise
      await fileChooser.setFiles([{ name, mimeType, buffer: Buffer.from(content) }])
    })
  }
}
