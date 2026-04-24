/** @file Actions common to all pages. */
import { expect, type Page } from 'integration-test/base'
import BaseActions, { type BaseActionsClass, type LocatorCallback } from './BaseActions'
import { openUserMenuAction } from './openUserMenuAction'
import { userMenuActions } from './userMenuActions'

/** Find right panel. */
function locateRightPanel(page: Page) {
  // This has no identifying features.
  return page.getByTestId('right-panel').locator('visible=true')
}

/** Actions common to all pages. */
export default class PageActions<
  Context,
  ParentClass extends BaseActionsClass<Context> = never,
> extends BaseActions<Context, ParentClass> {
  /** Actions related to the User Menu. */
  get userMenu() {
    return userMenuActions(this.step.bind(this))
  }

  /** Open the User Menu. */
  openUserMenu() {
    return openUserMenuAction(this.step.bind(this))
  }

  /** Show the properties tab of the Right Panel. */
  togglePropertiesAssetPanel() {
    return this.step('Toggle properties asset panel', async (page) => {
      await page.getByRole('tab', { name: 'Properties' }).click()
    })
  }

  /** Show the description tab of the Right Panel. */
  toggleDescriptionAssetPanel() {
    return this.step('Toggle description asset panel', async (page) => {
      await page.getByRole('tab', { name: 'Description' }).click()
    })
  }

  /** Expect Documentation Panel to be visible. */
  expectDocsPanel() {
    return this.step('Docs panel is opened', async (page) => {
      await expect(page.locator('.DocumentationEditor')).toBeVisible()
    })
  }

  /** Show the Docs tab of the Right Panel. */
  toggleDocsAssetPanel() {
    return this.step('Toggle docs asset panel', async (page) => {
      await page.getByRole('tab', { name: 'Documentation' }).click()
    })
  }

  /** Interact with the Right Panel. */
  withRightPanel(callback: LocatorCallback<Context>) {
    return this.step('Interact with right panel', async (page, context) => {
      await callback(locateRightPanel(page), context)
    })
  }
}
