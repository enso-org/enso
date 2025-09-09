/** @file Actions for the "account" form in settings. */
import type { Locator, Page } from 'integration-test/base'
import type { BaseActionsClass } from './BaseActions'
import PageActions from './PageActions'
import { TEXT } from './utilities'

/** Actions for the "account" form in settings. */
export default class SettingsFormActions<
  Context,
  ParentClass extends BaseActionsClass<Context>,
> extends PageActions<Context, ParentClass> {
  /** Construct a {@link SettingsFormActions}. */
  constructor(
    protected locate: (page: Page) => Locator,
    ...args: ConstructorParameters<typeof PageActions<Context, ParentClass>>
  ) {
    super(...args)
  }

  /** Save and submit this settings section. */
  save(waitForSave: boolean = true): InstanceType<ParentClass> {
    return this.step('Save settings form', async (page) => {
      const saveButton = this.locate(page).getByRole('button', { name: TEXT.save })

      await saveButton.getByText(TEXT.save).click()
      if (waitForSave) {
        await saveButton.waitFor({ state: 'detached' })
      }
    }).intoParent()
  }

  /** Cancel editing this settings section. */
  cancel(waitForCancel: boolean = true): InstanceType<ParentClass> {
    return this.step('Cancel editing settings form', async (page) => {
      const cancelButton = this.locate(page).getByRole('button', { name: TEXT.cancel })

      await cancelButton.getByText(TEXT.cancel).click()
      if (waitForCancel) {
        await cancelButton.waitFor({ state: 'detached' })
      }
    }).intoParent()
  }
}
