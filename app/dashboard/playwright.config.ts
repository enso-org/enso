/** @file Playwright browser testing configuration. */
/** Note that running Playwright in CI poses a number of issues:
 * - `backdrop-filter: blur` is disabled, due to issues with Chromium's `--disable-gpu` flag
 * (see below).
 * - System validation dialogs are not reliable between computers, as they may have different
 * default fonts. */
import * as test from '@playwright/test'

import * as appConfig from 'enso-common/src/appConfig'

appConfig.loadTestEnvironmentVariables()

/* eslint-disable @typescript-eslint/no-magic-numbers, @typescript-eslint/strict-boolean-expressions */

const DEBUG = process.env.PWDEBUG === '1'
const TIMEOUT_MS = DEBUG ? 100_000_000 : 30_000

export default test.defineConfig({
  testDir: './e2e',
  fullyParallel: true,
  forbidOnly: true,
  workers: process.env.PROD ? 8 : 1,
  repeatEach: process.env.CI ? 3 : 1,
  expect: {
    toHaveScreenshot: { threshold: 0 },
    timeout: TIMEOUT_MS,
  },
  timeout: TIMEOUT_MS,
  reporter: 'html',
  use: {
    baseURL: 'http://localhost:8080',
    trace: 'retain-on-failure',
    launchOptions: {
      ignoreDefaultArgs: ['--headless'],
      args: [
        ...(DEBUG ?
          []
        : [
            // Much closer to headful Chromium than classic headless.
            '--headless=new',
          ]),
        // Required for `backdrop-filter: blur` to work.
        '--use-angle=swiftshader',
        // FIXME: `--disable-gpu` disables `backdrop-filter: blur`, which is not handled by
        // the software (CPU) compositor. This SHOULD be fixed eventually, but this flag
        // MUST stay as CI does not have a GPU.
        '--disable-gpu',
        // Fully disable GPU process.
        '--disable-software-rasterizer',
        // Disable text subpixel antialiasing.
        '--font-render-hinting=none',
        '--disable-skia-runtime-opts',
        '--disable-system-font-check',
        '--disable-font-subpixel-positioning',
        '--disable-lcd-text',
      ],
    },
  },
  webServer: {
    command: `corepack pnpm run ${process.env.CI || process.env.PROD ? 'dev:e2e:ci' : 'dev:e2e'}`,
    port: 8080,
    reuseExistingServer: false,
  },
})
