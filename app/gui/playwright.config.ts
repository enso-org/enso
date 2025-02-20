/** @file Playwright browser testing configuration. */
/**
 * Note that running Playwright in CI poses a number of issues:
 * - `backdrop-filter: blur` is disabled, due to issues with Chromium's `--disable-gpu` flag
 * (see below).
 * - System validation dialogs are not reliable between computers, as they may have different
 * default fonts.
 */
import { defineConfig } from '@playwright/test'
import net from 'node:net'
import path from 'node:path'
import url from 'node:url'

const DEBUG = process.env.DEBUG_TEST === 'true'
const isCI = process.env.CI === 'true'
const isProd = process.env.PROD === 'true'
const TIMEOUT_MS = DEBUG ? 100_000_000 : 25_000

// We tend to use less CPU on CI to reduce the number of failures due to timeouts.
// Instead of using workers on CI, we use shards to run tests in parallel.
const WORKERS = isCI ? 2 : '35%'

const dirName = path.dirname(url.fileURLToPath(import.meta.url))

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

const portsFromEnv = {
  projectView: parseInt(process.env.PLAYWRIGHT_PORT_PV ?? '', 10),
  dashboard: parseInt(process.env.PLAYWRIGHT_PORT ?? '', 10),
}
const ports = {
  projectView:
    Number.isFinite(portsFromEnv.projectView) ?
      portsFromEnv.projectView
    : await findFreePortInRange(4300, 4999),
  dashboard:
    Number.isFinite(portsFromEnv.dashboard) ?
      portsFromEnv.dashboard
    : await findFreePortInRange(4300, 4999),
}
console.log(`Selected playwright servers' ports: ${ports.projectView} and ${ports.dashboard}`)
// Make sure to set the env to actual port that is being used. This is necessary for workers to
// pick up the same configuration.
process.env.PLAYWRIGHT_PORT = `${ports.dashboard}`
process.env.PLAYWRIGHT_PORT_PV = `${ports.projectView}`

export default defineConfig({
  fullyParallel: true,
  ...(WORKERS ? { workers: WORKERS } : {}),
  forbidOnly: isCI,
  reporter: isCI ? [['list'], ['blob']] : [['html']],
  retries: isCI ? 3 : 0,
  use: {
    headless: !DEBUG,
    actionTimeout: 5000,

    trace: 'retain-on-failure',
    ...(DEBUG ?
      {}
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
  projects: [
    // Setup project
    {
      name: 'Setup Dashboard',
      testDir: './integration-test/dashboard',
      testMatch: /.*\.setup\.ts/,
      timeout: TIMEOUT_MS,
      use: {
        baseURL: `http://localhost:${ports.dashboard}`,
        actionTimeout: TIMEOUT_MS,
        offline: false,
      },
    },
    {
      name: 'Dashboard',
      testDir: './integration-test/dashboard',
      testMatch: /.*\.spec\.ts/,
      dependencies: ['Setup Dashboard'],
      expect: {
        toHaveScreenshot: { threshold: 0 },
        timeout: TIMEOUT_MS,
      },
      timeout: TIMEOUT_MS,
      use: {
        baseURL: `http://localhost:${ports.dashboard}`,
        actionTimeout: TIMEOUT_MS,
        offline: false,
        storageState: path.join(dirName, './playwright/.auth/user.json'),
      },
    },
    {
      name: 'Setup Tests for Project View',
      testMatch: /integration-test\/project-view\/setup\.ts/,
    },
    {
      name: 'Project View',
      dependencies: ['Setup Tests for Project View'],
      testDir: './integration-test/project-view',
      timeout: 60000,
      repeatEach: 3,
      retries: 0,
      expect: {
        timeout: 5000,
        toHaveScreenshot: { threshold: 0 },
      },
      use: {
        viewport: { width: 1920, height: 1750 },
        baseURL: `http://localhost:${ports.projectView}`,
      },
    },
  ],
  webServer: [
    {
      env: {
        INTEGRATION_TEST: 'true',
        ENSO_IDE_PROJECT_MANAGER_URL: 'ws://__HOSTNAME__:30536',
      },
      command: `corepack pnpm build && corepack pnpm exec vite preview --port ${ports.projectView} --strictPort`,
      // Build from scratch apparently can take a while on CI machines.
      timeout: 240 * 1000,
      port: ports.projectView,
      // We use our special, mocked version of server, thus do not want to re-use user's one.
      reuseExistingServer: false,
    },
    {
      command:
        isCI || isProd ?
          `corepack pnpm exec vite -c vite.test.config.ts build && vite -c vite.test.config.ts preview --port ${ports.dashboard} --strictPort`
        : `NODE_ENV=test corepack pnpm exec vite -c vite.test.config.ts --port ${ports.dashboard}`,
      timeout: 240 * 1000,
      port: ports.dashboard,
      reuseExistingServer: false,
    },
  ],
})
