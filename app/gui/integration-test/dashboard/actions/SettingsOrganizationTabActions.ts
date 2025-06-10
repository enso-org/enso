/** @file Actions for the "organization" tab of the "settings" page. */
import BaseSettingsTabActions from './BaseSettingsTabActions'
import SettingsOrganizationFormActions from './SettingsOrganizationFormActions'
import { goToSettingsTabActions, type GoToSettingsTabActions } from './gotoSettingsTabActions'

/** Actions for the "organization" tab of the "settings" page. */
export default class SettingsOrganizationTabActions<
  Context,
> extends BaseSettingsTabActions<Context> {
  /** Actions for navigating to another settings tab. */
  get goToSettingsTab(): Omit<GoToSettingsTabActions<Context>, 'organization'> {
    return goToSettingsTabActions(this.step.bind(this))
  }

  /** Manipulate the "organization" form. */
  organizationForm() {
    return this.into(SettingsOrganizationFormActions<Context>)
  }

  /** Upload a profile picture. */
  uploadProfilePicture(
    name: string,
    content: WithImplicitCoercion<string | Uint8Array | readonly number[]>,
    mimeType: string,
  ) {
    return this.step('Upload organization profile picture', async (page) => {
      const fileChooserPromise = page.waitForEvent('filechooser')
      await page.getByTestId('organization-profile-picture-input').click()
      const fileChooser = await fileChooserPromise
      await fileChooser.setFiles([{ name, mimeType, buffer: Buffer.from(content) }])
    })
  }
}
