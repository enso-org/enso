/** @file Actions for the "settings" page. */
import type * as test from '@playwright/test'

import BaseActions from './BaseActions'

// ============================
// === SettingsModalActions ===
// ============================

// TODO: split settings page actions into different classes for each settings tab.
/** Actions for the "settings" page. */
export default class SettingsModalActions<
  PreviousStateClass extends new (
    page: test.Page,
    promise?: Promise<void>
  ) => InstanceType<PreviousStateClass>,
> extends BaseActions {
  /** Create a {@link SettingsModalActions}. */
  constructor(
    page: test.Page,
    promise: Promise<void>,
    private readonly previousStateClass: PreviousStateClass
  ) {
    super(page, promise)
  }

  /** Close this modal and go back to the Drive page. */
  close() {
    return this.step('Close "settings" modal', page => page.getByLabel('Close').click()).into(
      this.previousStateClass
    )
  }
}
