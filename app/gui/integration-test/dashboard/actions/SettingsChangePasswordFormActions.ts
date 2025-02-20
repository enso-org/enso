/** @file Actions for the "change password" form in settings. */
import { TEXT } from '.'
import type PageActions from './PageActions'
import SettingsAccountTabActions from './SettingsAccountTabActions'
import SettingsFormActions from './SettingsFormActions'

/** Actions for the "change password" form in settings. */
export default class SettingsChangePasswordFormActions<Context> extends SettingsFormActions<
  Context,
  typeof SettingsAccountTabActions<Context>
> {
  /** Create a {@link SettingsChangePasswordFormActions}. */
  constructor(...args: ConstructorParameters<typeof PageActions<Context>>) {
    super(
      SettingsAccountTabActions<Context>,
      (page) =>
        page
          .getByRole('heading')
          .and(page.getByText(TEXT.changePasswordSettingsSection))
          .locator('..'),
      ...args,
    )
  }

  /** Fill the "current password" input of this form. */
  fillCurrentPassword(name: string) {
    return this.step("Fill 'current password' input of 'change password' form", (page) =>
      this.locate(page)
        .getByLabel(TEXT.userCurrentPasswordSettingsInput)
        .getByRole('textbox')
        .fill(name),
    )
  }

  /** Fill the "new password" input of this form. */
  fillNewPassword(name: string) {
    return this.step("Fill 'new password' input of 'change password' form", (page) =>
      this.locate(page)
        .getByLabel(new RegExp('^' + TEXT.userNewPasswordSettingsInput))
        .getByRole('textbox')
        .fill(name),
    )
  }

  /** Fill the "confirm new password" input of this form. */
  fillConfirmNewPassword(name: string) {
    return this.step("Fill 'confirm new password' input of 'change password' form", (page) =>
      this.locate(page)
        .getByLabel(TEXT.userConfirmNewPasswordSettingsInput)
        .getByRole('textbox')
        .fill(name),
    )
  }
}
