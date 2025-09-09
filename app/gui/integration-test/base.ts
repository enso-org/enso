import { test as base, expect as baseExpect, type Locator } from 'playwright/test'
import type DrivePageActions from './actions/DrivePageActions'
import LoginPageActions from './actions/LoginPageActions'
import { mockCloudApi, type MockCloudApi } from './mock/cloudApi'
import { mockLocalApi, type MockLocalApi } from './mock/localApi'
import { registerMocks } from './mock/registerMocks'

export type * from 'playwright/test'

export interface PageCtx {
  // readonly cloudApi: MockCloudApi
  // readonly localApi: MockLocalApi
}

export const test = base.extend<{
  cloudApi: MockCloudApi
  localApi: MockLocalApi
  loginPage: LoginPageActions
  drivePage: DrivePageActions
}>({
  cloudApi: async ({ page }, use) => use(await mockCloudApi(page)),
  localApi: async ({ page }, use) => use(await mockLocalApi(page)),
  loginPage: ({ page, cloudApi, localApi }, use) => {
    // Only make sure that API mocks are registered, do not actually use the values
    const _ = { cloudApi, localApi }
    return use(new LoginPageActions(page, {}, registerMocks(page)))
  },
  drivePage: ({ loginPage }, use) => use(loginPage.loginIfNeeded()),
})

export const expect = baseExpect.extend({
  /**
   * Ensures that at least one of the elements that the Locator points to,
   * is an attached and visible DOM node.
   */
  async toExist(locator: Locator) {
    // Counter-intuitive, but correct:
    // https://playwright.dev/docs/api/class-locatorassertions#locator-assertions-to-be-visible
    const assertionName = 'toExist'
    let pass: boolean
    try {
      await expect(locator.first()).toBeVisible()
      pass = true
    } catch {
      pass = false
    }

    const message = () =>
      this.utils.matcherHint(assertionName, locator, '', {
        isNot: this.isNot,
      })

    return {
      message,
      pass,
      name: assertionName,
    }
  },

  async toHaveCountGreaterThan(locator: Locator, n: number) {
    const assertionName = 'toHaveCountGreaterThan'
    let pass: boolean
    try {
      await expect(locator.nth(n)).toBeVisible()
      pass = true
    } catch {
      pass = false
    }

    const message = () =>
      this.utils.matcherHint(assertionName, locator, '', {
        isNot: this.isNot,
      })

    return {
      message,
      pass,
      name: assertionName,
    }
  },

  async toBeSelected(locator: Locator) {
    const assertionName = 'toBeSelected'
    let pass: boolean
    try {
      await baseExpect(locator).toHaveClass(/(?<=^| )selected(?=$| )/, { timeout: 50 })
      pass = true
    } catch {
      // Do not log the error.
      pass = false
    }

    const message = () =>
      this.utils.matcherHint(assertionName, locator, '', {
        isNot: this.isNot,
      })

    return {
      message,
      pass,
      name: assertionName,
    }
  },
})
