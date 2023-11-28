/** @file Playwright browser testing configuration. */
/** Note that running Playwright in CI poses a number of issues:
 * - `backdrop-filter: blur` is disabled, due to issues with Chromium's `--disable-gpu` flag
 * (see below).
 * - System validation dialogs are not reliable between computers, as they may have different
 * default fonts. */
import { defineConfig } from '@playwright/test'

const DEBUG = process.env.DEBUG_E2E === 'true'

export default defineConfig({
  globalSetup: './e2e/setup.ts',
  testDir: './e2e',
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  ...(process.env.CI ? { workers: 1 } : {}),
  reporter: 'html',
  expect: {
    timeout: 5000,
    toHaveScreenshot: { threshold: 0 },
  },
  use: {
    headless: !DEBUG,
    baseURL: 'http://localhost:5173',
    trace: 'on-first-retry',
    ...(DEBUG
      ? {}
      : {
          launchOptions: {
            ignoreDefaultArgs: ['--headless'],
            args: [
              // Much closer to headful Chromium than classic headless.
              '--headless=new',
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
        }),
  },
  // projects: [
  // {
  //   name: 'chromium',
  //   use: {
  //     ...devices['Desktop Chrome'],
  //   },
  // },
  // {
  //   name: 'firefox',
  //   use: {
  //     ...devices['Desktop Firefox'],
  //   },
  // },
  // {
  //   name: 'webkit',
  //   use: {
  //     ...devices['Desktop Safari'],
  //   },
  // },
  // {
  //   name: 'Mobile Chrome',
  //   use: {
  //     ...devices['Pixel 5'],
  //   },
  // },
  // {
  //   name: 'Mobile Safari',
  //   use: {
  //     ...devices['iPhone 12'],
  //   },
  // },
  // ],
  webServer: {
    // The command-line flag is required
    command: process.env.CI ? 'E2E=true vite preview --port 5173' : 'E2E=true vite dev',
    port: 5173,
    reuseExistingServer: !process.env.CI,
  },
})
