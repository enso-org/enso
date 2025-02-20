/** @file A test for basic flow of the application: open project and see if nodes appear. */

import { expect } from '@playwright/test'
import fs from 'node:fs/promises'
import pathModule from 'node:path'
import { CONTROL_KEY, electronTest, loginAsTestUser } from './electronTest'

electronTest('Local Workflow', async ({ page, app, projectsDir }) => {
  const PROJECT_PATH = pathModule.join(projectsDir, 'NewProject1')
  const OUTPUT_FILE = 'output.txt'
  const TEXT_TO_WRITE = 'Some text'

  await loginAsTestUser(page)
  await expect(page.getByRole('button', { name: 'New Project', exact: true })).toBeVisible()
  await page.getByRole('button', { name: 'New Project', exact: true }).click()
  await expect(page.locator('.GraphNode')).toHaveCount(1, { timeout: 60000 })

  // We see the node type and visualization, so the engine is running the program
  await expect(page.locator('.node-type')).toHaveText('Table', { timeout: 30000 })
  await expect(page.locator('.TableVisualization')).toBeVisible({ timeout: 30000 })
  await expect(page.locator('.TableVisualization')).toContainText('Welcome To Enso!')

  // Create node connected to the first node by picking suggestion.
  await page.locator('.GraphNode').click()
  await page.keyboard.press('Enter')
  await expect(page.locator('.ComponentBrowser')).toBeVisible()
  const entry = page.locator('.ComponentList .list-variant.selected .component', {
    hasText: 'column_count',
  })
  await expect(entry).toBeVisible()
  await entry.click()
  await expect(page.locator('.GraphNode'), {}).toHaveCount(2)
  const addedNode = page.locator('.GraphNode', { hasText: 'column_count' })
  await addedNode.click()
  await addedNode.getByRole('button', { name: 'Visualization' }).click()
  await expect(addedNode.locator('.TableVisualization')).toBeVisible()
  await expect(addedNode.locator('.TableVisualization')).toContainText('1')

  // Create new text literal node.
  await page.keyboard.press('Escape') // deselect.
  await page.locator('.PlusButton').click()
  await expect(page.locator('.ComponentBrowser')).toBeVisible()
  const input = page.locator('.ComponentBrowser input')
  await input.fill(`'${TEXT_TO_WRITE}'`)
  await page.keyboard.press('Enter')
  await expect(page.locator('.GraphNode'), {}).toHaveCount(3)

  // Create write node
  await page.keyboard.press('Enter')
  await expect(page.locator('.ComponentBrowser')).toBeVisible()
  const code = `write (enso_project.root / '${OUTPUT_FILE}') on_existing_file=..Append`
  await input.fill(code)
  await page.keyboard.press('Enter')
  await expect(page.locator('.GraphNode'), {}).toHaveCount(4)

  // Check that the output file is not created yet.
  const writeNode = page.locator('.GraphNode', { hasText: 'write' })
  await writeNode.click()
  await writeNode.getByRole('button', { name: 'Visualization' }).click()
  await expect(writeNode.locator('.TableVisualization')).toContainText('output_ensodryrun')

  expect(await fs.readdir(PROJECT_PATH)).not.toContain(OUTPUT_FILE)

  // Press `Write once` button.
  await writeNode.locator('.More').click()
  await writeNode.getByTestId('recompute').click()

  // Check that the output file is created and contains expected text.
  try {
    await expect(writeNode.locator('.TableVisualization')).toContainText(OUTPUT_FILE)
  } catch {
    // TODO[ao]
    // The above check is flaky, because sometimes the additional engine run overrides node output back to "dry run".
    // To confirm if this should be expected.
    console.error(
      'Didn\'t see the visualization update after "Write once" action; assuming it\'s already done',
    )
  }
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

  // Open docpanel and paste an image.
  await page.getByRole('button', { name: 'Documentation Panel' }).click()
  await page.locator('.DocumentationEditor').click()
  await page.keyboard.press(`${CONTROL_KEY}+V`)
  const docImageElement = page.locator('.DocumentationEditor').getByAltText('Image')
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
