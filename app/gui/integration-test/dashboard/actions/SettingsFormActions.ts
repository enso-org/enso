/** @file Actions for the "account" form in settings. */
import type { Locator, Page } from '@playwright/test'
import { TEXT } from '.'
import type { BaseActionsClass } from './BaseActions'
import PageActions from './PageActions'

/** Actions for the "account" form in settings. */
export default class SettingsFormActions<
  Context,
  ParentClass extends BaseActionsClass<Context>,
> extends PageActions<Context> {
  /** Construct a {@link SettingsFormActions}. */
  constructor(
    private parentClass: ParentClass,
    protected locate: (page: Page) => Locator,
    ...args: ConstructorParameters<typeof PageActions<Context>>
  ) {
    super(...args)
  }

  /** Save and submit this settings section. */
  save(): InstanceType<ParentClass> {
    return this.step('Save settings form', (page) =>
      this.locate(page).getByRole('button', { name: TEXT.save }).getByText(TEXT.save).click(),
    ).into(this.parentClass)
  }

  /** Cancel editing this settings section. */
  cancel(): InstanceType<ParentClass> {
    return this.step('Cancel editing settings form', (page) =>
      this.locate(page).getByRole('button', { name: TEXT.cancel }).getByText(TEXT.cancel).click(),
    ).into(this.parentClass)
  }
}
