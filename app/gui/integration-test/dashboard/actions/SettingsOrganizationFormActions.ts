/** @file Actions for the "organization" form in settings. */
import { TEXT } from '.'
import type { LocatorCallback } from './BaseActions'
import type PageActions from './PageActions'
import SettingsFormActions from './SettingsFormActions'
import SettingsOrganizationTabActions from './SettingsOrganizationTabActions'

/** Actions for the "organization" form in settings. */
export default class SettingsOrganizationFormActions<Context> extends SettingsFormActions<
  Context,
  typeof SettingsOrganizationTabActions<Context>
> {
  /** Create a {@link SettingsOrganizationFormActions}. */
  constructor(...args: ConstructorParameters<typeof PageActions<Context>>) {
    super(
      SettingsOrganizationTabActions<Context>,
      (page) =>
        page
          .getByRole('heading')
          .and(page.getByText(TEXT.organizationSettingsSection))
          .locator('..'),
      ...args,
    )
  }

  /** Fill the "name" input of this form. */
  fillName(name: string) {
    return this.step("Fill 'name' input of 'organization' form", (page) =>
      this.locate(page)
        .getByLabel(TEXT.organizationNameSettingsInput)
        .getByRole('textbox')
        .fill(name),
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

  /** Fill the "email" input of this form. */
  fillEmail(name: string) {
    return this.step("Fill 'email' input of 'organization' form", (page) =>
      this.locate(page)
        .getByLabel(TEXT.organizationEmailSettingsInput)
        .getByRole('textbox')
        .fill(name),
    )
  }

  /** Interact with the "email" input of this form. */
  withEmail(callback: LocatorCallback<Context>) {
    return this.step("Interact with 'email' input of 'organization' form", (page, context) =>
      callback(
        this.locate(page).getByLabel(TEXT.organizationEmailSettingsInput).getByRole('textbox'),
        context,
      ),
    )
  }

  /** Fill the "website" input of this form. */
  fillWebsite(name: string) {
    return this.step("Fill 'website' input of 'organization' form", (page) =>
      this.locate(page)
        .getByLabel(TEXT.organizationWebsiteSettingsInput)
        .getByRole('textbox')
        .fill(name),
    )
  }

  /** Interact with the "website" input of this form. */
  withWebsite(callback: LocatorCallback<Context>) {
    return this.step("Interact with 'website' input of 'organization' form", (page, context) =>
      callback(
        this.locate(page).getByLabel(TEXT.organizationWebsiteSettingsInput).getByRole('textbox'),
        context,
      ),
    )
  }

  /** Fill the "location" input of this form. */
  fillLocation(name: string) {
    return this.step("Fill 'location' input of 'organization' form", (page) =>
      this.locate(page)
        .getByLabel(TEXT.organizationLocationSettingsInput)
        .getByRole('textbox')
        .fill(name),
    )
  }

  /** Interact with the "location" input of this form. */
  withLocation(callback: LocatorCallback<Context>) {
    return this.step("Interact with 'name' input of 'organization' form", (page, context) =>
      callback(
        this.locate(page).getByLabel(TEXT.organizationLocationSettingsInput).getByRole('textbox'),
        context,
      ),
    )
  }
}
