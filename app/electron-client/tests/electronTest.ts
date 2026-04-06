/** @file Commonly used functions for electron tests */
import { TEXTS } from 'enso-common/src/text'
import fs from 'node:fs/promises'
import os from 'node:os'
import path from 'node:path'
import {
  _electron,
  expect,
  type ElectronApplication,
  type Locator,
  type Page,
  type TestInfo,
} from 'playwright/test'

const LOADING_TIMEOUT = 10000
const TEXT = TEXTS.english
const RUNFILES_WORKSPACE_ROOT =
  process.env.JS_BINARY__RUNFILES ? path.join(process.env.JS_BINARY__RUNFILES, '_main') : undefined

function uniquePaths(paths: readonly (string | undefined)[]): string[] {
  return [
    ...new Set(paths.filter((value): value is string => value != null).map((p) => path.resolve(p))),
  ]
}

function workspacePathCandidates(relativeOrAbsolutePath: string): string[] {
  if (path.isAbsolute(relativeOrAbsolutePath)) {
    return [relativeOrAbsolutePath]
  }
  return uniquePaths([
    path.resolve(process.cwd(), relativeOrAbsolutePath),
    RUNFILES_WORKSPACE_ROOT ?
      path.join(RUNFILES_WORKSPACE_ROOT, relativeOrAbsolutePath)
    : undefined,
  ])
}

const TEST_USER_FILE_CANDIDATES = uniquePaths([
  path.join(import.meta.dirname, '../playwright/.auth/user.json'),
  path.join(process.cwd(), 'playwright/.auth/user.json'),
  ...workspacePathCandidates('app/electron-client/playwright/.auth/user.json'),
])

const POSSIBLE_ELECTRON_DIRS = uniquePaths([
  ...(process.env.ENSO_EXEC_PATH ? workspacePathCandidates(process.env.ENSO_EXEC_PATH) : []),
  path.join(import.meta.dirname, '../ide-dist'),
  path.join(import.meta.dirname, '../../../dist/ide'),
])

const POSSIBLE_ELECTRON_PATHS = POSSIBLE_ELECTRON_DIRS.flatMap((dir) => [
  path.join(dir, 'linux-unpacked/enso'),
  path.join(dir, 'win-unpacked/Enso.exe'),
  path.join(dir, 'mac/Enso.app/Contents/MacOS/Enso'),
  path.join(dir, 'mac-arm64/Enso.app/Contents/MacOS/Enso'),
])

type TestCredentials = { readonly user: string; readonly password: string }

async function readCredentialsFile(filePath: string): Promise<TestCredentials> {
  const contents = await fs.readFile(filePath, { encoding: 'utf-8' })
  const parsed = JSON.parse(contents) as Partial<TestCredentials>
  if (!parsed.user || !parsed.password) {
    throw new Error(`Missing 'user' or 'password' key in '${filePath}'.`)
  }
  return { user: parsed.user, password: parsed.password }
}

async function loadCredentials(): Promise<TestCredentials> {
  let lastError: unknown = undefined
  for (const candidate of TEST_USER_FILE_CANDIDATES) {
    try {
      return await readCredentialsFile(candidate)
    } catch (error) {
      lastError = error
      const code = typeof error === 'object' && error != null && 'code' in error ? error.code : null
      if (code !== 'ENOENT') {
        console.warn(`[pw:credentials] Failed to read '${candidate}':`, error)
      }
    }
  }

  if (process.env.ENSO_TEST_USER && process.env.ENSO_TEST_PASS) {
    return { user: process.env.ENSO_TEST_USER, password: process.env.ENSO_TEST_PASS }
  }

  throw new Error(
    `Cannot load test credentials. Set ENSO_TEST_USER and ENSO_TEST_PASS, or create one of: ${TEST_USER_FILE_CANDIDATES.join(', ')}`,
    { cause: lastError },
  )
}

export const credentials = await loadCredentials()

let cachedElectronPath: string | undefined

export async function getElectronExecutablePath(): Promise<string | undefined> {
  if (cachedElectronPath !== undefined) return cachedElectronPath
  try {
    const promises = POSSIBLE_ELECTRON_PATHS.map((p) =>
      fs.access(p, fs.constants.X_OK).then(() => p),
    )
    cachedElectronPath = await Promise.any(promises)
    return cachedElectronPath
  } catch {
    return undefined
  }
}

/**
 * Custom test fixtures for Electron tests.
 * Spec files should import `test` directly from `playwright/test` and extend it
 * with these fixtures to avoid dual module instance issues with Bazel.
 * @example
 * ```ts
 * import { test as base, expect } from 'playwright/test'
 * import { electronFixtures, loginAsTestUser } from './electronTest'
 *
 * const test = base.extend(electronFixtures)
 *
 * test('my test', async ({ page }) => {
 *   await loginAsTestUser(page)
 * })
 * ```
 */
export const electronFixtures = {
  // eslint-disable-next-line no-empty-pattern
  testRunId: async function ({}, use: (value: string) => Promise<void>, testInfo: TestInfo) {
    await use(`${testInfo.titlePath.join('-')}-${Date.now()}`)
  },
  projectsDir: async function (
    { testRunId }: { testRunId: string },
    use: (value: string) => Promise<void>,
  ) {
    const projectsDir = path.join(os.tmpdir(), 'enso-test-projects', testRunId)
    await fs.mkdir(projectsDir, { recursive: true })
    await use(projectsDir)
  },

  /** Setup for all tests: Create an electron-based app instance. */
  app: async function (
    { projectsDir, testRunId }: { projectsDir: string; testRunId: string },
    use: (value: ElectronApplication) => Promise<void>,
    testInfo: TestInfo,
  ) {
    const args = process.env.ENSO_TEST_APP_ARGS?.split(',') ?? []
    const executablePath = await getElectronExecutablePath()
    if (!executablePath) {
      throw new Error('Cannot find Enso package executable')
    }
    const app = await _electron.launch({
      executablePath,
      args,
      env: {
        ...process.env,
        HOME: process.env.HOME ?? os.homedir(),
        ENSO_TEST: 'true',
        ENSO_TEST_PARTITION: `enso-test-${testRunId}`,
        ENSO_TEST_PROJECTS_DIR: projectsDir.replace(/\\/g, '/'),
      },
    })
    // Set the password as global var before turning on tracing.
    // This way it will be not disclosed to anyone downloading traces of failed tests.
    ;(await app.firstWindow()).evaluate((password) => {
      ;(window as any).passwordOverride = password
    }, credentials.password)
    const context = app.context()
    const tracePath = testInfo.outputPath('trace.zip')
    await context.tracing.start({ screenshots: true, snapshots: true, sources: true })
    try {
      await use(app)
    } finally {
      const shouldSaveTrace = testInfo.status !== testInfo.expectedStatus
      if (shouldSaveTrace) {
        await context.tracing.stop({ path: tracePath })
        await testInfo.attach('trace', { path: tracePath, contentType: 'application/zip' })
      } else {
        await context.tracing.stop()
      }
      await app.close()
    }
  },
  page: async function (
    {
      app,
      viewport,
    }: { app: ElectronApplication; viewport?: { width: number; height: number } | null },
    use: (value: Page) => Promise<void>,
  ) {
    const innerPage = await app.firstWindow()
    if (viewport) innerPage.setViewportSize(viewport)
    await use(innerPage)
  },
}

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
    projects.map(async (p: Locator) => {
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

/** Click a locator even when Playwright cannot bring it into the viewport. */
export async function clickWithoutViewportConstraints(locator: Locator) {
  const target = locator.first()
  await expect(target).toBeVisible()
  await target.dispatchEvent('click', { bubbles: true, cancelable: true })
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
  await clickWithoutViewportConstraints(page.getByText(parentComponent, { exact: true }))
  await page.keyboard.press('Enter')
}

/** Select a component browser entry without relying on viewport-based mouse input. */
export async function selectComponentEntry(page: Page, name: string | RegExp) {
  await clickWithoutViewportConstraints(page.locator('.ComponentEntry', { hasText: name }))
}

/** Open component browser without relying on viewport-bound right click. */
export async function openComponentBrowserFromGraphNode(
  page: Page,
  parentComponent: string,
  occurrence = 0,
) {
  const target = page.getByText(parentComponent, { exact: true }).nth(occurrence)
  await expect(target).toBeVisible()
  await target.dispatchEvent('contextmenu', {
    button: 2,
    buttons: 2,
    bubbles: true,
    cancelable: true,
  })
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
export function openDropdownInWidget(page: Page, label: string) {
  return clickWithoutViewportConstraints(
    page.locator('.WidgetSelection', { hasText: new RegExp(`^${label}$`) }),
  )
}

/** Find and click + button in an empty Vector Widget inside provided locator. */
export function addFirstElementToWidgetVector(locator: Locator) {
  return locator.getByRole('list').filter({ hasText: /^$/ }).getByLabel('Add a new item').click()
}
