/** @file Actions for the "account" form in settings. */
import { TEXT } from '.'
import type { LocatorCallback } from './BaseActions'
import type PageActions from './PageActions'
import SettingsAccountTabActions from './SettingsAccountTabActions'
import SettingsFormActions from './SettingsFormActions'

/** Actions for the "account" form in settings. */
export default class SettingsAccountFormActions<Context> extends SettingsFormActions<
  Context,
  typeof SettingsAccountTabActions<Context>
> {
  /** Create a {@link SettingsAccountFormActions}. */
  constructor(...args: ConstructorParameters<typeof PageActions<Context>>) {
    super(
      SettingsAccountTabActions<Context>,
      (page) =>
        page
          .getByRole('heading')
          .and(page.getByText(TEXT.userAccountSettingsSection))
          .locator('..'),
      ...args,
    )
  }

  /** Fill the "name" input of this form. */
  fillName(name: string) {
    return this.step("Fill 'name' input of 'account' form", (page) =>
      this.locate(page).getByLabel(TEXT.userNameSettingsInput).getByRole('textbox').fill(name),
    )
  }

  /** Interact with the "name" input of this form. */
  withName(callback: LocatorCallback<Context>) {
    return this.step("Interact with 'name' input of 'organization' form", (page, context) =>
      callback(
        this.locate(page).getByLabel(TEXT.organizationNameSettingsInput).getByRole('textbox'),
        context,
      ),
    )
  }
}
