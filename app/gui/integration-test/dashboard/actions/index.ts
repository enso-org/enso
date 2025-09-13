/** @file Various actions, locators, and constants used in end-to-end tests. */
import { getText as baseGetText, type Replacements, type TextId } from 'enso-common/src/text'
import path from 'node:path'
import url from 'node:url'
import { expect, test, type Page } from 'playwright/test'
import { INITIAL_CALLS_OBJECT, mockApi, type MockApi, type TrackedCalls } from './api'
// Also necessary as a hack to avoid circular import errors.
import DrivePageActions from './DrivePageActions'
import LATEST_GITHUB_RELEASES from './latestGithubReleases.json' with { type: 'json' }
import {
  INITIAL_LOCAL_CALLS_OBJECT,
  mockLocalApi,
  type LocalMockApi,
  type LocalTrackedCalls,
} from './localApi'
import LoginPageActions from './LoginPageActions'
import { passAgreementsDialog, TEXT, type MockParams } from './utilities'
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
async function loginIfNeeded(page: Page, actions: LoginPageActions<Context>) {
  const authFile = getAuthFilePath()
  const isLoggedIn = (await page.getByTestId('before-auth-layout').count()) === 0
  if (isLoggedIn) {
    test.info().annotations.push({
      type: 'skip',
      description: 'Already logged in',
    })
    const agreementModalVisible = (await page.locator('#agreements-modal').count()) > 0
    if (agreementModalVisible) {
      await passAgreementsDialog({ page })
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

/** A placeholder date for visual regression testing. */
const MOCK_DATE = Number(new Date('01/23/45 01:23:45'))

/** Replace `Date` with a version that returns a fixed time. */
async function mockDate({ page }: MockParams) {
  // https://github.com/microsoft/playwright/issues/6347#issuecomment-1085850728
  await test.step('Mock Date', async () => {
    await page.addInitScript(`{
        Date = class extends Date {
            constructor(...args) {
                if (args.length === 0) {
                    super(${MOCK_DATE});
                } else {
                    super(...args);
                }
            }
        }
        const __DateNowOffset = ${MOCK_DATE} - Date.now();
        const __DateNow = Date.now;
        Date.now = () => __DateNow() + __DateNowOffset;
    }`)
  })
}

interface Context {
  readonly api: MockApi
  readonly localApi: LocalMockApi
  calls: TrackedCalls
  localCalls: LocalTrackedCalls
}

/** Set up all mocks, without logging in. */
export function mockAll({ page, setupAPI, setupLocalAPI }: MockParams) {
  const context: { -readonly [K in keyof Context]: Context[K] } = {
    api: undefined!,
    localApi: undefined!,
    calls: INITIAL_CALLS_OBJECT,
    localCalls: INITIAL_LOCAL_CALLS_OBJECT,
  }
  return new LoginPageActions<Context>(page, context)
    .step('Execute all mocks', async (page) => {
      await Promise.all([
        mockApi({ page, setupAPI }).then((api) => {
          context.api = api
        }),
        mockLocalApi({ page, setupLocalAPI }).then((localApi) => {
          context.localApi = localApi
        }),
        mockDate({ page }),
        mockUnneededUrls({ page }),
      ])
    })
    .step('Navigate to the root page', async (page) => {
      await page.goto('/')
      await waitForLoaded(page)
    })
}

export interface MockAllAndLoginParams extends MockParams {}

/** Set up all mocks, and log in with dummy credentials. */
export function mockAllAndLogin({
  page,
  setupAPI,
  setupLocalAPI,
  goToCloudFirst = true,
}: MockAllAndLoginParams) {
  const actions = mockAll({ page, setupAPI, setupLocalAPI })

  const driveActions = actions
    .step('Pass login screen', (page, _ctx, actions) => loginIfNeeded(page, actions))
    .step('Wait for dashboard to load', waitForDashboardToLoad)
    .into(DrivePageActions<Context>)
  return goToCloudFirst ? driveActions.goToCategory.cloud() : driveActions
}

/** Mock unneeded URLs. */
async function mockUnneededUrls({ page }: MockParams) {
  const eulaJsonBody = JSON.stringify({
    path: '/eula.md',
    size: 9472,
    modified: '2024-05-21T10:47:27.000Z',
    hash: '1c8a655202e59f0efebf5a83a703662527aa97247052964f959a8488382604b8',
  })
  const privacyJsonBody = JSON.stringify({
    path: '/privacy.md',
    size: 1234,
    modified: '2024-05-21T10:47:27.000Z',
    hash: '1c8a655202e59f0efebf5a83a703662527aa97247052964f959a8488382604b8',
  })

  await test.step('Mock unneeded URLs', async () => {
    return Promise.all([
      page.route('https://cdn.enso.org/**', async (route) => {
        await route.fulfill()
      }),

      page.route('https://www.google-analytics.com/**', async (route) => {
        await route.fulfill()
      }),

      page.route('https://www.googletagmanager.com/gtag/js*', async (route) => {
        await route.fulfill({ contentType: 'text/javascript', body: 'export {};' })
      }),

      page.route('https://*.ingest.sentry.io/api/*/envelope/*', async (route) => {
        await route.fulfill()
      }),

      page.route('https://api.mapbox.com/mapbox-gl-js/*/mapbox-gl.css', async (route) => {
        await route.fulfill({ contentType: 'text/css', body: '' })
      }),

      page.route('https://ensoanalytics.com/eula.json', async (route) => {
        await route.fulfill({ contentType: 'text/json', body: eulaJsonBody })
      }),

      page.route('https://ensoanalytics.com/privacy.json', async (route) => {
        await route.fulfill({ contentType: 'text/json', body: privacyJsonBody })
      }),

      page.route('https://fonts.googleapis.com/css2*', async (route) => {
        await route.fulfill({ contentType: 'text/css', body: '' })
      }),

      page.route('https://api.github.com/repos/enso-org/enso/releases/latest', async (route) => {
        await route.fulfill({ json: LATEST_GITHUB_RELEASES })
      }),

      page.route('https://github.com/enso-org/enso/releases/download/**', async (route) => {
        await route.fulfill({
          status: 200,
          headers: {
            'content-type': 'text/html',
          },
          body: '<meta http-equiv="Refresh" content="0; URL=https://objects.githubusercontent.com/foo/bar" />',
        })
      }),

      page.route('https://objects.githubusercontent.com/**', async (route) => {
        await route.fulfill({
          status: 200,
          headers: {
            'content-type': 'application/octet-stream',
            'last-modified': 'Wed, 24 Jul 2024 17:22:47 GMT',
            etag: '"0x8DCAC053D058EA5"',
            server: 'Windows-Azure-Blob/1.0 Microsoft-HTTPAPI/2.0',
            'x-ms-request-id': '20ab2b4e-c01e-0068-7dfa-dd87c5000000',
            'x-ms-version': '2020-10-02',
            'x-ms-creation-time': 'Wed, 24 Jul 2024 17:22:47 GMT',
            'x-ms-lease-status': 'unlocked',
            'x-ms-lease-state': 'available',
            'x-ms-blob-type': 'BlockBlob',
            'content-disposition': 'attachment; filename=enso-linux-x86_64-2024.3.1-rc3.AppImage',
            'x-ms-server-encrypted': 'true',
            via: '1.1 varnish, 1.1 varnish',
            'accept-ranges': 'bytes',
            age: '1217',
            date: 'Mon, 29 Jul 2024 09:40:09 GMT',
            'x-served-by': 'cache-iad-kcgs7200163-IAD, cache-bne12520-BNE',
            'x-cache': 'HIT, HIT',
            'x-cache-hits': '48, 0',
            'x-timer': 'S1722246008.269342,VS0,VE895',
            'content-length': '1030383958',
          },
        })
      }),
    ])
  })
}
