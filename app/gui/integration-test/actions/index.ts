/** @file Various actions, locators, and constants used in end-to-end tests. */
import { getText as baseGetText, type Replacements, type TextId } from 'enso-common/src/text'
import path from 'node:path'
import url from 'node:url'
import { expect, test, type Page } from 'playwright/test'
import { INITIAL_CALLS_OBJECT, type MockCloudApi, type TrackedCalls } from '../mock/cloudApi'
// Also necessary as a hack to avoid circular import errors.
import { registerAllMocks, type MockParams } from 'integration-test/mock/registerMocks'
import {
  INITIAL_LOCAL_CALLS_OBJECT,
  type LocalTrackedCalls,
  type MockLocalApi,
} from '../mock/localApi'
import DrivePageActions from './DrivePageActions'
import LoginPageActions from './LoginPageActions'
import { passAgreementsDialog, TEXT } from './utilities'
export * from './utilities'

export const getText = (key: TextId, ...replacements: Replacements[TextId]) => {
  return baseGetText(TEXT, key, ...replacements)
}

/** Get the path to the auth file. */
export function getAuthFilePath() {
  const __dirname = path.dirname(url.fileURLToPath(import.meta.url))
  return path.join(__dirname, '../../../playwright/.auth/user.json')
}

/** Perform a successful login. */
async function loginIfNeeded(page: Page, actions: LoginPageActions<CommonContext>) {
  const authFile = getAuthFilePath()
  const isLoggedIn = (await page.getByTestId('before-auth-layout').count()) === 0
  if (isLoggedIn) {
    test.info().annotations.push({
      type: 'skip',
      description: 'Already logged in',
    })
    const agreementModalVisible = (await page.locator('#agreements-modal').count()) > 0
    if (agreementModalVisible) {
      await passAgreementsDialog(page)
      await page.context().storageState({ path: authFile })
    }
  } else {
    await actions.login()
    await page.context().storageState({ path: authFile })
  }
}

/** Wait for the page to load. */
async function waitForLoaded(page: Page) {
  await page.waitForLoadState()

  await expect(page.getByTestId(/^(before|after)-auth-layout$/)).toBeAttached({ timeout: 30_000 })
  await expect(page.getByTestId('loading-screen')).toHaveCount(0, { timeout: 30_000 })
}

/** Wait for the dashboard to load. */
async function waitForDashboardToLoad(page: Page) {
  await waitForLoaded(page)
  await expect(page.getByTestId('after-auth-layout')).toBeAttached()
}

export interface CommonContext {
  readonly cloudApi: MockCloudApi
  readonly localApi: MockLocalApi
  calls: TrackedCalls
  localCalls: LocalTrackedCalls
}

/** Set up all mocks, without logging in. */
export function mockAll(mockParams: MockParams): LoginPageActions<CommonContext> {
  const context: { -readonly [K in keyof CommonContext]: CommonContext[K] } = {
    cloudApi: undefined!,
    localApi: undefined!,
    calls: INITIAL_CALLS_OBJECT,
    localCalls: INITIAL_LOCAL_CALLS_OBJECT,
  }

  async function initLoginPage() {
    await test.step('Execute all mocks', () => registerAllMocks(mockParams))
    await test.step('Navigate to the root page', async () => {
      await mockParams.page.goto('/')
      await waitForLoaded(mockParams.page)
    })
  }

  return new LoginPageActions<CommonContext>(mockParams.page, context, initLoginPage())
}

export interface MockAllAndLoginParams extends MockParams {
  readonly goToCloudFirst?: boolean
}

/** Set up all mocks, and log in with dummy credentials. */
export function mockAllAndLogin(params: MockAllAndLoginParams) {
  const actions = mockAll(params)

  const driveActions = actions
    .step('Pass login screen', async (page, _ctx, actions) => {
      await loginIfNeeded(page, actions)
      await waitForDashboardToLoad(page)
    })
    .into(DrivePageActions<CommonContext>)
  return params.goToCloudFirst !== false ? driveActions.goToCategory.cloud() : driveActions
}
