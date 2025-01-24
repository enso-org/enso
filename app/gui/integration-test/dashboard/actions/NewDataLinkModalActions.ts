/** @file Actions for a "new Data Link" modal. */
import type { Page } from '@playwright/test'

import { TEXT } from '.'
import BaseActions, { type LocatorCallback } from './BaseActions'
import DrivePageActions from './DrivePageActions'

/** Locate the "new data link" modal. */
function locateNewDataLinkModal(page: Page) {
  return page.getByRole('dialog').filter({ has: page.getByText('Create Datalink') })
}

/** Actions for a "new Data Link" modal. */
export default class NewDataLinkModalActions<Context> extends BaseActions<Context> {
  /** Cancel creating the new Data Link (don't submit the form). */
  cancel(): DrivePageActions<Context> {
    return this.step('Cancel out of "new data link" modal', async () => {
      await this.press('Escape')
    }).into(DrivePageActions<Context>)
  }

  /** Interact with the "name" input - for example, to set the name using `.fill("")`. */
  withNameInput(callback: LocatorCallback<Context>) {
    return this.step('Interact with "name" input', async (page, context) => {
      const locator = locateNewDataLinkModal(page).getByPlaceholder(TEXT.datalinkNamePlaceholder)
      await callback(locator, context)
    })
  }
}
