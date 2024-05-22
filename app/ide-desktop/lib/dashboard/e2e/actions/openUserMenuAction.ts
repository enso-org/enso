/** @file An action to open the User Menu. */
import type * as baseActions from './BaseActions'
import type BaseActions from './BaseActions'

// ==========================
// === openUserMenuAction ===
// ==========================

/** An action to open the User Menu. */
export function openUserMenuAction<T extends BaseActions>(
  step: (name: string, callback: baseActions.PageCallback) => T
) {
  return step('Open user menu', page =>
    page.getByAltText('Open user menu').locator('visible=true').click()
  )
}
