/** @file Actions for a "new Data Link" modal. */
import type * as test from 'playwright/test'

import { TEXT } from '.'
import type * as baseActions from './BaseActions'
import BaseActions from './BaseActions'
import DrivePageActions from './DrivePageActions'

// ==============================
// === locateNewDataLinkModal ===
// ==============================

/** Locate the "new data link" modal. */
function locateNewDataLinkModal(page: test.Page) {
  return page.getByRole('dialog').filter({ has: page.getByText('Create Datalink') })
}

// ===============================
// === NewDataLinkModalActions ===
// ===============================

/** Actions for a "new Data Link" modal. */
export default class NewDataLinkModalActions extends BaseActions {
  /** Cancel creating the new Data Link (don't submit the form). */
  cancel() {
    return this.step('Cancel out of "new data link" modal', async () => {
      await this.press('Escape')
    }).into(DrivePageActions)
  }

  /** Interact with the "name" input - for example, to set the name using `.fill("")`. */
  withNameInput(callback: baseActions.LocatorCallback) {
    return this.step('Interact with "name" input', async (page) => {
      const locator = locateNewDataLinkModal(page).getByPlaceholder(TEXT.datalinkNamePlaceholder)
      await callback(locator)
    })
  }
}
