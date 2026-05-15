/** @file Commonly used functions for electron tests */
import { TEXTS } from 'enso-common/src/text'
import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'
import {
  _electron,
  test as base,
  expect,
  type ElectronApplication,
  type Locator,
  type Page,
} from 'playwright/test'

const LOADING_TIMEOUT = 10000
const TEXT = TEXTS.english
const TEST_USER_FILE = path.join(import.meta.dirname, '../playwright/.auth/user.json')
const POSSIBLE_ELECTRON_PATHS = [
  '../../../dist/ide/linux-unpacked/enso',
  '../../../dist/ide/win-unpacked/Enso.exe',
  '../../../dist/ide/mac/Enso.app/Contents/MacOS/Enso',
  '../../../dist/ide/mac-arm64/Enso.app/Contents/MacOS/Enso',
]

export const credentials: { readonly user: string; readonly password: string } = await fs
  .readFile(TEST_USER_FILE, { encoding: 'utf-8' })
  .then(
    (contents) => JSON.parse(contents),
    (error) => {
      throw new Error(`Cannot read Test User credentials from '${TEST_USER_FILE}'.`, {
        cause: error,
      })
    },
  )
  .catch((error) => {
    throw new Error(`Cannot parse Test User credentials from '${TEST_USER_FILE}'.`, {
      cause: error,
    })
  })

export const electronExecutablePath = await (async () => {
  try {
    const promises = POSSIBLE_ELECTRON_PATHS.map((p) => path.resolve(import.meta.dirname, p)).map(
      (p) => fs.access(p, fs.constants.X_OK).then(() => p),
    )
    return await Promise.any(promises)
  } catch {
    throw Error('Cannot find Enso package')
  }
})()

/**
 * Tests run on electron executable.
 *
 * Similar to playwright's test, but launches electron, and passes Page of the main window.
 */
export const test = base.extend<{
  testRunId: string
  projectsDir: string
  /**
   * Whether the launched Electron process should spawn the local Claude agent. Default `false`
   * — the test fixture sets `ENSO_AI_DISABLED=1` so a developer's locally-installed `claude` is
   * not accidentally invoked during a non-AI spec. AI specs override with
   * `test.use({ aiEnabled: true })`.
   */
  aiEnabled: boolean
  app: ElectronApplication
  page: Page
}>({
  // eslint-disable-next-line no-empty-pattern
  testRunId: async function ({}, use, testInfo) {
    await use(`${testInfo.titlePath.join('-')}-${Date.now()}`)
  },
  projectsDir: async function ({ testRunId }, use) {
    const projectsDir = path.join(os.tmpdir(), 'enso-test-projects', testRunId)
    await use(projectsDir)
  },
  aiEnabled: [false, { option: true }],

  /** Setup for all tests: Create an electron-based app instance. */
  app: async function ({ projectsDir, testRunId, aiEnabled }, use) {
    const args = process.env.ENSO_TEST_APP_ARGS?.split(',') ?? []
    const app = await _electron.launch({
      executablePath: electronExecutablePath,
      args,
      env: {
        ...process.env,
        ENSO_TEST: 'true',
        ENSO_TEST_PROJECTS_DIR: projectsDir.replace(/\\/g, '/'),
        ...(aiEnabled ? {} : { ENSO_AI_DISABLED: '1' }),
      },
    })
    // Set the password as global var before turning on tracing.
    // This way it will be not disclosed to anyone downloading traces of failed tests.
    ;(await app.firstWindow()).evaluate((password) => {
      ;(window as any).passwordOverride = password
    }, credentials.password)
    await app.context().tracing.start({ screenshots: true, snapshots: true, sources: true })
    await use(app)
    await app.context().tracing.stop({ path: `test-traces/${testRunId}.zip` })
    await app.close()
  },
  page: async function ({ app, viewport }, use) {
    const innerPage = await app.firstWindow()
    if (viewport) innerPage.setViewportSize(viewport)
    await use(innerPage)
  },
})

/**
 * Login as test user - assert that page is the login page, and use credentials from
 * `playwright/.auth/user.json`.
 */
export async function loginAsTestUser(page: Page) {
  // Login screen
  await expect(page.getByText('Login to your account')).toBeVisible({ timeout: LOADING_TIMEOUT })
  await expect(page.getByRole('textbox', { name: 'email' })).toBeVisible()
  await expect(page.getByRole('textbox', { name: 'password' })).toBeVisible()
  await page.getByRole('textbox', { name: 'email' }).fill(credentials.user)
  // Put some placeholder - the actual password was set in fixture (see above).
  await page.getByRole('textbox', { name: 'password' }).fill('mellon')
  await page.getByRole('button', { name: TEXT.login, exact: true }).click()

  await expect(
    page
      .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
      .getByText(TEXT.licenseAgreementCheckbox),
  ).toBeVisible({ timeout: 60000 })
  await page
    .getByRole('group', { name: TEXT.licenseAgreementCheckbox })
    .getByText(TEXT.licenseAgreementCheckbox)
    .click()
  await page
    .getByRole('group', { name: TEXT.privacyPolicyCheckbox })
    .getByText(TEXT.privacyPolicyCheckbox)
    .click()

  await page.getByRole('button', { name: TEXT.accept }).click()
}

/** Create a new Enso project */
export async function createNewProject(page: Page) {
  await page.getByRole('button', { name: 'New Project' }).click()
  await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

  const tableViz = page.locator('.TableVisualization')
  await expect(tableViz).toContainText('Welcome To Enso!')
}

/** If welcome project is to be opened, navigate back to the dashboard. */
export async function closeWelcome(page: Page) {
  const welcomeProjectTab = page.getByRole('tab', { name: 'Getting Started with Enso Analytics' })
  const loadingIndicator = welcomeProjectTab.locator('.LoadingSpinner')
  await Promise.race([
    welcomeProjectTab
      .waitFor({ state: 'visible', timeout: 0 })
      .then(() => loadingIndicator.waitFor({ state: 'hidden' })),
    page.waitForTimeout(3000),
  ])
  if (await welcomeProjectTab.isVisible()) {
    await welcomeProjectTab.locator('.CloseButton').click()
  }
}

/**
 * Finds the "newest" project (highest numbered "New Project N") in the user dasboard.
 * @param page - The Playwright Page instance
 * @returns Locator for the newest project
 */
export async function getNewestProject(page: Page): Promise<Locator> {
  await expect(page.getByTestId('drive-view')).toBeVisible({ timeout: LOADING_TIMEOUT })
  const projectsLocator = page.getByTestId('drive-view').getByText(/New Project \d+/)
  await expect(projectsLocator).not.toHaveCount(0)

  const projects = await page
    .getByTestId('drive-view')
    .getByText(/New Project \d+/)
    .all()

  const numbered = await Promise.all(
    projects.map(async (p) => {
      const text = await p.innerText()
      const num = parseInt(text.replace('New Project ', ''), 10)
      return { locator: p, num }
    }),
  )
  return numbered.reduce((a, b) => (a.num > b.num ? a : b)).locator
}

/**
 * Click the eye button, visualizing component data
 */
export async function visualizeData(page: Page) {
  const showViz = page.getByLabel('Show visualization (Space)')
  await showViz.click({ timeout: 5000 })
}

/**
 * Open new component browser refefencing the last created component
 */
export async function createNewComponent(page: Page) {
  const moreButton = page.getByTestId('more-button').getByRole('button', { name: 'More' }).last()
  await moreButton.click()

  await page.keyboard.press('Enter')
}

/**
 * Open new component browser based on the name of referenced parent component
 */
export async function openComponentBrowser(page: Page, parentComponent: string) {
  await page.getByText(parentComponent, { exact: true }).click()
  await page.keyboard.press('Enter')
}

/**
 * Find textbox located in parent component and fill in text value
 */
export async function fillWidgetText(
  page: Page,
  containerName: string,
  value: string,
  index?: number,
) {
  const cont = page.getByText(containerName)

  const box = cont.getByTestId('widget-text-content')
  if (index) return box.nth(index).fill(value)
  else return box.fill(value)
}

/**
 * Wait for the Samples folder download
 * This function retries to access passed file every 5 sec, fails after 1 min
 */
export async function waitForDownload(pathToFile: string): Promise<void> {
  const start = Date.now()
  while (true) {
    try {
      await fs.access(pathToFile) // ✅ file exists
      return
    } catch {
      if (Date.now() - start > 60_000) {
        throw new Error(`File ${pathToFile} not found within 60 seconds`)
      }
      await new Promise((r) => setTimeout(r, 5_000))
    }
  }
}

/** Open drop-down menu in WidgetSelection with given label. */
export async function openDropdownInWidget(page: Page, label: string) {
  await page.locator('.WidgetSelection', { hasText: new RegExp(`^${label}$`) }).click()
  // Wait for any in-flight dropdown transitions to finish before returning, so subsequent
  // item clicks don't race with a leaving/entering animation.
  await expect(page.locator('.DropdownWidget[data-transitioning]')).toHaveCount(0)
}

/** Find and click + button in an empty Vector Widget inside provided locator. */
export function addFirstElementToWidgetVector(locator: Locator) {
  return locator.getByRole('list').filter({ hasText: /^$/ }).getByLabel('Add a new item').click()
}

/**
 * Multiplier applied to `maxIdleMs` while the placeholder bubble is showing the "Waiting…" /
 * "Waiting (#N)" labels. That state covers both the renderer-side queue (a previous prompt is
 * still in flight) and the main-process wait on `claude` priming (~36 stdlib reads on the first
 * session start, several minutes wall clock). Both can legitimately outlast the regular
 * `maxIdleMs` for reasons unrelated to a stuck model, so apply a looser threshold while the
 * bubble is in that state — a 10× factor matches the old `PRIMING_TIMEOUT_MS = 600 s` priming
 * cap when callers pass the standard `maxIdleMs: 60_000`.
 */
const WAITING_IDLE_FACTOR = 10

/**
 * Run `action` (typically a Playwright assertion waiting on an AI-generated node to commit) but
 * fail fast when the visible AI placeholder bubble — `data-testid="ai-pending-status"` inside a
 * non-failed `.AiPendingNode` — sits on the same text for `maxIdleMs`. Mirrors what a user
 * staring at the bubble would notice and matches the system prompt's "max 60 s between feedback"
 * contract, so a stuck turn surfaces immediately instead of burning the action's full per-prompt
 * budget. The baseline resets while no active placeholder is on screen, so the watchdog only
 * counts time during which a turn is genuinely in flight. While the bubble shows the
 * "Waiting…" label the threshold is multiplied by {@link WAITING_IDLE_FACTOR} (queue/priming
 * legitimately outlast the regular budget).
 */
export async function withFeedbackWatchdog(
  page: Page,
  opts: { maxIdleMs: number },
  action: () => Promise<void>,
): Promise<void> {
  const placeholderText = page.locator(
    '.AiPendingNode:not(.failed) [data-testid="ai-pending-status"]',
  )
  let lastText = ''
  let lastChangeAt = Date.now()
  let watchdogReject!: (err: Error) => void
  const watchdog = new Promise<never>((_, reject) => {
    watchdogReject = reject
  })
  const intervalId = setInterval(async () => {
    try {
      const count = await placeholderText.count()
      if (count === 0) {
        lastText = ''
        lastChangeAt = Date.now()
        return
      }
      const text = (await placeholderText.first().textContent()) ?? ''
      if (text !== lastText) {
        lastText = text
        lastChangeAt = Date.now()
        return
      }
      const idleFor = Date.now() - lastChangeAt
      const effectiveMax =
        text.startsWith('Waiting') ? opts.maxIdleMs * WAITING_IDLE_FACTOR : opts.maxIdleMs
      if (idleFor > effectiveMax) {
        watchdogReject(
          new Error(
            `AI placeholder text "${text}" unchanged for ${idleFor} ms (max ${effectiveMax} ms)`,
          ),
        )
      }
    } catch {
      // Locator queries can race a DOM update mid-tick; ignore and retry next interval. Real
      // bugs surface either via the `action` promise or the next tick's stall detection.
    }
  }, 1000)
  try {
    await Promise.race([action(), watchdog])
  } finally {
    clearInterval(intervalId)
  }
}
