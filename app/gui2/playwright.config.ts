/** @file Playwright browser testing configuration. */
/** Note that running Playwright in CI poses a number of issues:
 * - `backdrop-filter: blur` is disabled, due to issues with Chromium's `--disable-gpu` flag
 * (see below).
 * - System validation dialogs are not reliable between computers, as they may have different
 * default fonts. */
import { defineConfig } from '@playwright/test'
import net from 'net'

const DEBUG = process.env.DEBUG_E2E === 'true'

async function findFreePortInRange(min: number, max: number) {
  for (let i = 0; i < 50; i++) {
    const portToCheck = Math.floor(Math.random() * (max - min + 1)) + min
    if (await checkAvailablePort(portToCheck)) return portToCheck
  }
  throw new Error('Failed to find a free port.')
}

function checkAvailablePort(port: number) {
  return new Promise((resolve, reject) => {
    const server = net.createServer()
    server
      .unref()
      .on('error', (e: any) => ('EADDRINUSE' === e.code ? resolve(false) : reject(e)))
      .listen({ host: '0.0.0.0', port }, () => server.close(() => resolve(true)))
  })
}

const portFromEnv = parseInt(process.env.PLAYWRIGHT_PORT ?? '', 10)
const PORT = Number.isFinite(portFromEnv) ? portFromEnv : await findFreePortInRange(4300, 4999)
// Make sure to set the env to actual port that is being used. This is necessary for workers to
// pick up the same configuration.
process.env.PLAYWRIGHT_PORT = `${PORT}`

export default defineConfig({
  globalSetup: './e2e/setup.ts',
  testDir: './e2e',
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  ...(process.env.CI ? { workers: 1 } : {}),
  reporter: 'line',
  expect: {
    timeout: 5000,
    toHaveScreenshot: { threshold: 0 },
  },
  use: {
    headless: !DEBUG,
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
    env: {
      E2E: 'true',
    },
    command: `npx vite build && npx vite preview --port ${PORT} --strictPort`,
    // Build from scratch apparently can take a while on CI machines.
    timeout: 120 * 1000,
    port: PORT,
    // We use our special, mocked version of server, thus do not want to re-use user's one.
    reuseExistingServer: false,
  },
})
