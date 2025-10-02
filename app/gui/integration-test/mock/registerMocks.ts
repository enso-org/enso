import type { FeatureFlags } from '$/providers/featureFlags'
import { test } from 'integration-test/base'
import type { Page, Route } from 'playwright'
import LATEST_GITHUB_RELEASES from './data/latestGithubReleases.json' with { type: 'json' }

/** Execute registration hooks for all playwright mocks that are shared across all tests. */
export async function registerMocks(
  page: Page,
  featureFlags: Partial<FeatureFlags>,
): Promise<void> {
  await Promise.all([mockDate(page), mockUnneededUrls(page), mockFeatureFlags(page, featureFlags)])
}

/** A placeholder date for visual regression testing. */
const MOCK_DATE = Number(new Date('01/23/45 01:23:45'))

async function mockFeatureFlags(page: Page, featureFlags: Partial<FeatureFlags>) {
  const flags = Object.assign(
    {
      enableLocalBackend: true,
      enableCloudExecution: true,
      enableAdvancedProjectExecutionOptions: true,
      enableAssetsTableBackgroundRefresh: false,
    },
    featureFlags,
  )

  await test.step('Set feature flags', async () => {
    return page.addInitScript((flags) => {
      if ('overrideFeatureFlags' in window) {
        Object.assign(window.overrideFeatureFlags, flags)
      } else {
        Object.defineProperty(window, 'overrideFeatureFlags', {
          value: flags,
          writable: false,
          configurable: false,
        })
      }
    }, flags)
  })
}

/** Replace `Date` with a version that returns a fixed time. */
async function mockDate(page: Page) {
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

/** Mock unneeded URLs. */
async function mockUnneededUrls(page: Page) {
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
    const fulfillWith =
      (...options: Parameters<Route['fulfill']>) =>
      (r: Route) =>
        r.fulfill(...options)
    const empty = fulfillWith()

    return Promise.all([
      page.route('https://cdn.enso.org/**', empty),
      page.route('https://www.google-analytics.com/**', empty),
      page.route('https://www.googletagmanager.com/gtag/js*', async (route) => {
        await route.fulfill({ contentType: 'text/javascript', body: 'export {};' })
      }),

      page.route('https://*.ingest.sentry.io/api/*/envelope/*', empty),
      page.route(
        'https://api.mapbox.com/mapbox-gl-js/*/mapbox-gl.css',
        fulfillWith({ contentType: 'text/css', body: '' }),
      ),
      page.route(
        'https://ensoanalytics.com/eula.json',
        fulfillWith({ contentType: 'text/json', body: eulaJsonBody }),
      ),
      page.route(
        'https://ensoanalytics.com/privacy.json',
        fulfillWith({ contentType: 'text/json', body: privacyJsonBody }),
      ),
      page.route(
        'https://fonts.googleapis.com/css2*',
        fulfillWith({ contentType: 'text/css', body: '' }),
      ),
      page.route(
        'https://api.github.com/repos/enso-org/enso/releases/latest',
        fulfillWith({ json: LATEST_GITHUB_RELEASES }),
      ),
      page.route(
        'https://github.com/enso-org/enso/releases/download/**',
        fulfillWith({
          status: 200,
          headers: {
            'content-type': 'text/html',
          },
          body: '<meta http-equiv="Refresh" content="0; URL=https://objects.githubusercontent.com/foo/bar" />',
        }),
      ),
      page.route(
        'https://objects.githubusercontent.com/**',
        fulfillWith({
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
        }),
      ),
    ])
  })
}
