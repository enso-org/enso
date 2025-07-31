/** @file A test for basic flow of the application: open project and see if nodes appear. */

import fs from 'node:fs/promises'
import pathModule from 'node:path'
import { type Page, expect } from 'playwright/test'
import { CONTROL_KEY, loginAsTestUser, test } from './electronTest'

const startTimestamp = Date.now()
let screenshotIndex = 0
async function _doScreenshot(page: Page): Promise<void> {
  page.screenshot({ path: `test-traces/screenshots/${startTimestamp}/${screenshotIndex++}.png` })
}

async function writeToFocusedComponentBrowser(page: Page, content: string): Promise<void> {
  await expect(page.locator('.ComponentBrowser')).toBeVisible()
  const input = page.getByTestId('component-editor-content')
  await expect(input).toBeFocused()
  await input.fill(content)
}

test('Local Workflow', async ({ page, app, projectsDir }) => {
  const OUTPUT_FILE = 'output.txt'
  const TEXT_TO_WRITE = 'Some text'

  await loginAsTestUser(page)
  await expect(page.getByRole('button', { name: 'New Project', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'New Project', exact: true }).click()
  await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

  const projectName = (await page.getByTitle('Project Name').textContent()) ?? ''
  await expect(projectName).toBeTruthy()
  const PROJECT_PATH = pathModule.join(projectsDir, projectName.replaceAll(' ', ''))

  // We see the node type and visualization, so the engine is running the program
  await expect(page.locator('.node-type')).toHaveText('Table & +3', { timeout: 30000 })
  await expect(page.locator('.TableVisualization')).toBeVisible({ timeout: 30000 })
  await expect(page.locator('.TableVisualization')).toContainText('Welcome To Enso!')

  // Create node connected to the first node by picking suggestion.
  await page.locator('.GraphNode').click()
  await page.keyboard.press('Enter')
  await writeToFocusedComponentBrowser(page, 'count')
  const entry = page.locator('.ComponentEntry', { hasText: 'column_count' })
  await expect(entry).toBeVisible()
  await await entry.click()
  await expect(page.locator('.GraphNode'), {}).toHaveCount(2)
  const addedNode = page.locator('.GraphNode', { hasText: 'column_count' })
  await addedNode.click()
  await addedNode.getByRole('button', { name: 'Visualization' }).click()
  await expect(addedNode.locator('.TableVisualization')).toBeVisible()
  await expect(addedNode.locator('.TableVisualization')).toContainText('1')

  // Select nodes and create User Defined Component nodes
  await page.keyboard.press(`${CONTROL_KEY}+A`)
  await page
    .getByRole('button', { name: 'Create User Defined Component from Selected Components' })
    .click()
  await expect(page.locator('.GraphNode')).toHaveCount(1)
  await expect(page.locator('.GraphNode')).toHaveText(/Main.user_defined_component/)
  await page.locator('.GraphNode').click()
  await page.getByRole('button', { name: 'Visualization' }).click()
  await expect(page.locator('.TableVisualization')).toBeVisible()
  await expect(page.locator('.TableVisualization')).toContainText('1')

  // Enter User Defined Component
  // First wait until node is computed. Visualization may be cached, so we look at icon.
  await expect(page.locator('.GraphNode .WidgetIcon svg use')).toHaveAttribute('href', /#group/)
  await page.locator('.GraphNode').dblclick()
  await expect(page.locator('.GraphNode')).toHaveCount(3)
  await expect(page.locator('.NavBreadcrumb')).toHaveText([
    'New Project 1',
    'user_defined_component',
  ])

  // Rename User Defined component
  await page.getByRole('tab', { name: 'Documentation' }).click()
  await page
    .locator('.FunctionSignatureEditor')
    .getByTestId('widget-function-name-content')
    .dblclick() // double click for select all.
  await page.keyboard.insertText('new_name')
  await page.keyboard.press('Enter')
  await expect(page.locator('.NavBreadcrumb')).toHaveText(['New Project 1', 'new_name'])

  // Leave function
  await page.mouse.dblclick(10, 100)
  await expect(page.locator('.GraphNode')).toHaveCount(1)
  await expect(page.locator('.GraphNode')).toHaveText(/Main.new_name/)

  // Create new text literal node.
  await page.keyboard.press('Escape') // deselect.
  await page.getByTestId('add-component-button').click()
  await writeToFocusedComponentBrowser(page, `'${TEXT_TO_WRITE}'`)
  await page.keyboard.press('Enter')
  await expect(page.locator('.GraphNode'), {}).toHaveCount(2)

  // Create write node
  await page.keyboard.press('Enter')
  await writeToFocusedComponentBrowser(
    page,
    `write (enso_project.root / '${OUTPUT_FILE}') on_existing_file=..Append`,
  )
  await page.keyboard.press('Enter')
  await expect(page.locator('.GraphNode'), {}).toHaveCount(3)

  // Check that the output file is not created yet.
  const writeNode = page.locator('.GraphNode', { hasText: 'write' })
  await writeNode.click()
  await writeNode.getByRole('button', { name: 'Visualization' }).click()
  await expect(writeNode.locator('.TableVisualization')).toContainText('output_ensodryrun')

  expect(await fs.readdir(PROJECT_PATH)).not.toContain(OUTPUT_FILE)
  await expect(page.locator('.GraphEditor .GraphNode.pending')).toHaveCount(0)
  // Press `Write once` button.
  await writeNode.locator('.More').click()
  await writeNode.getByTestId('action:component.recompute').click()
  await page.mouse.move(0, 0) // Avoid showing a tooltip
  await expect(page.locator('.GraphEditor .GraphNode.pending')).toHaveCount(0)

  // Check that the output file is created and contains expected text.
  await expect(writeNode.locator('.TableVisualization')).toContainText(OUTPUT_FILE)
  let projectFiles = await fs.readdir(PROJECT_PATH)
  expect(projectFiles).toContain(OUTPUT_FILE)
  if (projectFiles.includes(OUTPUT_FILE)) {
    const content = await readFile(PROJECT_PATH, OUTPUT_FILE)
    expect(content).toStrictEqual(TEXT_TO_WRITE)
  }

  // Put an image to clipboard.
  await app.evaluate(({ nativeImage, clipboard }) => {
    const image = nativeImage.createFromDataURL(
      'data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAMAAAADBAMAAACkBqiMAAAAFVBMVEX+MjP+Kyv6bGz7X1/9Ojr+Li7+JyfwctYrAAAAEUlEQVQI12NwVWBgE2BgNAAAA6EArVWjc3wAAAAASUVORK5CYII=',
    )
    clipboard.writeImage(image)
  })

  // Paste an image in documentation.
  // (the panel is opened in previous steps)
  await page.locator('.DocumentationEditor').click()
  await page.keyboard.press(`${CONTROL_KEY}+V`)
  const docImageElement = page.locator('.DocumentationEditor').getByTestId('doc-img')
  await expect(docImageElement).toBeVisible()
  await expect(docImageElement).toHaveJSProperty('width', 3)

  // Image is properly uploaded.
  // Wait for upload; while uploading, the image is a bit transparent.
  await expect(docImageElement).not.toHaveClass(/[$ ]uploading[^ ]/, { timeout: 10000 })
  projectFiles = await fs.readdir(PROJECT_PATH)
  expect(projectFiles).toContain('images')
  const images = await fs.readdir(pathModule.join(PROJECT_PATH, 'images'))
  expect(images).toContain('image.png')
})

async function readFile(projectDir: string, fileName: string): Promise<string> {
  return await fs.readFile(pathModule.join(projectDir, fileName), 'utf8')
}
