/** @file An action to open the User Menu. */
import type { default as BaseActions, BaseActionsClass, PageCallback } from './BaseActions'
import { TEXT } from './utilities'

/** An action to open the User Menu. */
export function openUserMenuAction<
  T extends BaseActions<Context, ParentClass>,
  Context,
  ParentClass extends BaseActionsClass<Context>,
>(step: (name: string, callback: PageCallback<Context, T>) => T) {
  return step('Open user menu', (page) =>
    page.getByLabel(TEXT.userMenuLabel).locator('visible=true').click(),
  )
}
