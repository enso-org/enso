import type { FeatureFlags } from '$/providers/featureFlags'
import { UUID } from 'enso-common/src/services/Backend'
import { toRfc3339 } from 'enso-common/src/utilities/data/dateTime'
import { test as base, expect as baseExpect, type Locator } from 'playwright/test'
import type DrivePageActions from './actions/DrivePageActions'
import type EditorPageActions from './actions/EditorPageActions'
import LoginPageActions from './actions/LoginPageActions'
import { mockCloudApi, type MockCloudApi } from './mock/cloudApi'
import { mockLocalApi, type MockLocalApi } from './mock/localApi'
import { registerMocks } from './mock/registerMocks'

export type * from 'playwright/test'

export const test = base.extend<{
  featureFlags: Partial<FeatureFlags>
  /**
   * Whether the mocked Electron API should report the local Claude agent as available. Most
   * specs assume `false`. AI specs opt in with `test.use({ aiAvailable: true })`.
   */
  aiAvailable: boolean
  setupApi: {
    cloud?: (api: MockCloudApi) => void
    local?: (api: MockLocalApi) => void
    addDefaultProject?: boolean
  }
  cloudApi: MockCloudApi
  localApi: MockLocalApi
  loginPage: LoginPageActions
  drivePage: DrivePageActions
  editorPage: EditorPageActions
}>({
  featureFlags: [{}, { option: true }],
  aiAvailable: [false, { option: true }],
  setupApi: [{}, { option: true }],
  cloudApi: async ({ page, setupApi }, use) => {
    const api = await mockCloudApi(page)
    setupApi.cloud?.(api)
    return use(api)
  },
  localApi: async ({ page, setupApi }, use) => {
    const api = await mockLocalApi(page)
    if (setupApi?.addDefaultProject ?? true) {
      api.addProject({
        metadata: {
          id: UUID('135af445-bcfb-42fe-aa74-96f95e99c28b'),
          name: 'Mock Project',
          namespace: 'local',
          created: toRfc3339(new Date()),
        },
      })
    }

    setupApi.local?.(api)
    return use(api)
  },
  loginPage: async ({ page, cloudApi, localApi, featureFlags, aiAvailable }, use) => {
    // Only make sure that API mocks are registered, do not actually use the values
    const _ = { cloudApi, localApi }
    await registerMocks(page, featureFlags, { aiAvailable })

    const loginPage = new LoginPageActions(page, {}).do(async () => {
      await page.goto('/')
    })
    await use(await loginPage)
    return
  },
  drivePage: async ({ loginPage }, use) => await use(await loginPage.loginIfNeeded()),

  editorPage: async ({ drivePage }, use) => {
    const editorPage = drivePage.driveTable
      .openProject('Mock Project')
      .expectProjectEditorOpened('Mock Project')
      .expectNodePositionsInitialized()
      .expectDocsPanel()
      .toggleDocsAssetPanel()
    await use(await editorPage)
    return
  },
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
