/** @file Actions for the "home" page. */
import type { Page } from '@playwright/test'
import BaseActions, { type LocatorCallback } from './BaseActions'
import DrivePageActions from './DrivePageActions'
import EditorPageActions from './EditorPageActions'

/** Find a samples list. */
function locateSamplesList(page: Page) {
  // This has no identifying features.
  return page.getByTestId('samples')
}

/** Find all samples list. */
function locateSamples(page: Page) {
  // This has no identifying features.
  return locateSamplesList(page).getByRole('button')
}

/** Actions for the "start" modal. */
export default class StartModalActions<Context> extends BaseActions<Context> {
  /** Close this modal and go back to the Drive page. */
  close() {
    return this.step('Close start modal', async (page) => {
      const isOnScreen = await this.isStartModalShown(page)
      if (isOnScreen) {
        await this.locateStartModal(page).getByTestId('close-button').click()
      }
    }).into(DrivePageActions<Context>)
  }

  /** Locate the "start" modal. */
  private locateStartModal(page: Page) {
    return page.getByTestId('start-modal')
  }

  /** Check if the Asset Panel is shown. */
  private isStartModalShown(page: Page) {
    return this.locateStartModal(page)
      .isHidden()
      .then(
        (result) => !result,
        () => false,
      )
  }

  /** Create a project from the template at the given index. */
  createProjectFromTemplate(index: number) {
    return this.step(`Create project from template #${index}`, (page) =>
      locateSamples(page)
        .nth(index + 1)
        .click(),
    ).into(EditorPageActions<Context>)
  }

  /** Interact with the "start" modal. */
  withStartModal(callback: LocatorCallback<Context>) {
    return this.step('Interact with start modal', async (page, context) => {
      await callback(this.locateStartModal(page), context)
    })
  }
}
