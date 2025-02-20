/** @file An action to open the User Menu. */
import { TEXT } from '.'
import type BaseActions from './BaseActions'
import type { PageCallback } from './BaseActions'

/** An action to open the User Menu. */
export function openUserMenuAction<T extends BaseActions<Context>, Context>(
  step: (name: string, callback: PageCallback<Context>) => T,
) {
  return step('Open user menu', (page) =>
    page.getByLabel(TEXT.userMenuLabel).locator('visible=true').click(),
  )
}
