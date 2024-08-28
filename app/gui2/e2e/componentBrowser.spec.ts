import { test, type Page } from '@playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { CONTROL_KEY } from './keyboard'
import * as locate from './locate'

const ACCEPT_INPUT_SHORTCUT = `${CONTROL_KEY}+Enter`

async function deselectAllNodes(page: Page) {
  await page.keyboard.press('Escape')
  await expect(locate.selectedNodes(page)).toHaveCount(0)
}

async function expectAndCancelBrowser(
  page: Page,
  expectedText: string,
  expectedSelfArgument?: string,
) {
  const nodeCount = await locate.graphNode(page).count()
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserEntry(page)).toExist()
  if (expectedSelfArgument != null)
    await expect(locate.componentBrowser(page)).toHaveAttribute(
      'data-self-argument',
      expectedSelfArgument,
    )
  await expect(locate.componentBrowserInput(page).locator('input')).toHaveValue(expectedText)
  await expect(locate.componentBrowserInput(page).locator('input')).toBeInViewport()
  await page.keyboard.press('Escape')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount)
}

test('Different ways of opening Component Browser', async ({ page }) => {
  await actions.goToGraph(page)

  // Without source node

  // (+) button
  await locate.addNewNodeButton(page).click()
  await expectAndCancelBrowser(page, '')
  // (+) button with selection (ignored)
  await locate.graphNodeByBinding(page, 'final').click()
  await locate.addNewNodeButton(page).click()
  await expectAndCancelBrowser(page, '')
  // Enter key
  await locate.graphEditor(page).press('Enter')
  await expectAndCancelBrowser(page, '')

  // With source node

  // Enter key
  await locate.graphNodeByBinding(page, 'final').click()
  await locate.graphEditor(page).press('Enter')
  await expectAndCancelBrowser(page, '', 'final')
  // Dragging out an edge
  const outputPort = await locate.outputPortCoordinates(locate.graphNodeByBinding(page, 'final'))
  await page.mouse.click(outputPort.x, outputPort.y)
  await locate.graphEditor(page).click({ position: { x: 100, y: 500 } })
  await expectAndCancelBrowser(page, '', 'final')
  // Double-clicking port
  // TODO[ao] Without timeout, even the first click would be treated as double due to previous
  // event. Probably we need a better way to simulate double clicks.
  await page.waitForTimeout(600)
  await page.mouse.click(outputPort.x, outputPort.y)
  await page.mouse.click(outputPort.x, outputPort.y)
  await expectAndCancelBrowser(page, '', 'final')
})

test('Opening Component Browser with small plus buttons', async ({ page }) => {
  await actions.goToGraph(page)

  // Small (+) button shown when node is hovered
  await page.keyboard.press('Escape')
  await page.mouse.move(100, 80)
  await expect(locate.smallPlusButton(page)).toBeHidden()
  await locate.graphNodeIcon(locate.graphNodeByBinding(page, 'selected')).hover()
  await expect(locate.smallPlusButton(page)).toBeVisible()
  await locate.smallPlusButton(page).click()
  await expectAndCancelBrowser(page, '', 'selected')

  // Small (+) button shown when node is sole selection
  await page.keyboard.press('Escape')
  await page.mouse.move(300, 300)
  await expect(locate.smallPlusButton(page)).toBeHidden()
  await locate.graphNodeByBinding(page, 'selected').click()
  await expect(locate.smallPlusButton(page)).toBeVisible()
  await locate.smallPlusButton(page).click()
  await expectAndCancelBrowser(page, '', 'selected')
})

test('Graph Editor pans to Component Browser', async ({ page }) => {
  await actions.goToGraph(page)

  // Select node, pan out of view of it, press Enter; should pan to show node and CB
  await locate.graphNodeByBinding(page, 'final').click()
  await page.mouse.move(100, 80)
  await page.mouse.down({ button: 'middle' })
  await page.mouse.move(100, 1200)
  await page.mouse.up({ button: 'middle' })
  await expect(locate.graphNodeByBinding(page, 'final')).not.toBeInViewport()
  await locate.graphEditor(page).press('Enter')
  await expect(locate.graphNodeByBinding(page, 'final')).toBeInViewport()
  await expectAndCancelBrowser(page, '', 'final')

  // Dragging out an edge to the bottom of the viewport; when the CB pans into view, some nodes are out of view.
  await page.mouse.move(100, 1100)
  await page.mouse.down({ button: 'middle' })
  await page.mouse.move(100, 280)
  await page.mouse.up({ button: 'middle' })
  await expect(locate.graphNodeByBinding(page, 'five')).toBeInViewport()
  const outputPort = await locate.outputPortCoordinates(locate.graphNodeByBinding(page, 'final'))
  await page.mouse.click(outputPort.x, outputPort.y)
  await locate.graphEditor(page).click({ position: { x: 100, y: 1700 } })
  await expect(locate.graphNodeByBinding(page, 'five')).not.toBeInViewport()
  await expectAndCancelBrowser(page, '', 'final')
})

test('Accepting suggestion', async ({ page }) => {
  // Clicking entry
  await actions.goToGraph(page)
  await locate.addNewNodeButton(page).click()
  let nodeCount = await locate.graphNode(page).count()
  await locate.componentBrowserEntry(page).nth(1).click()
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText([
    'Data',
    '.',
    'read_text',
  ])
  await expect(locate.graphNode(page).last()).toBeSelected()

  // Clicking at highlighted entry
  nodeCount = await locate.graphNode(page).count()
  await deselectAllNodes(page)
  await locate.addNewNodeButton(page).click()
  await locate.componentBrowserSelectedEntry(page).first().click()
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText([
    'Data',
    '.',
    'read',
  ])
  await expect(locate.graphNode(page).last()).toBeSelected()

  // Accepting with Enter
  nodeCount = await locate.graphNode(page).count()
  await deselectAllNodes(page)
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserEntry(page)).toExist()
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText([
    'Data',
    '.',
    'read',
  ])
  await expect(locate.graphNode(page).last()).toBeSelected()
})

test('Accepting any written input', async ({ page }) => {
  await actions.goToGraph(page)
  await locate.addNewNodeButton(page).click()
  const nodeCount = await locate.graphNode(page).count()
  await locate.componentBrowserInput(page).locator('input').fill('re')
  await page.keyboard.press(ACCEPT_INPUT_SHORTCUT)
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount + 1)
  await expect(locate.graphNode(page).last().locator('.WidgetToken')).toHaveText('re')
})

test('Filling input with suggestion', async ({ page }) => {
  await actions.goToGraph(page)
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserEntry(page)).toExist()

  // Applying suggestion
  await page.keyboard.press('Tab')
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserInput(page).locator('input')).toHaveValue('Data.read ')
})

test('Filtering list', async ({ page }) => {
  await actions.goToGraph(page)
  await locate.addNewNodeButton(page).click()
  await locate.componentBrowserInput(page).locator('input').fill('re_te')
  const segments = locate.componentBrowserEntry(page).locator('.component-label-segment')
  await expect(segments).toHaveText(['Data.', 're', 'ad', '_te', 'xt'])
  const highlighted = locate.componentBrowserEntry(page).locator('.component-label-segment.match')
  await expect(highlighted).toHaveText(['re', '_te'])
})

test('Editing existing nodes', async ({ page }) => {
  await actions.goToGraph(page)
  const node = locate.graphNodeByBinding(page, 'data')
  const ADDED_PATH = '"/home/enso/Input.txt"'

  // Start node editing
  await locate.graphNodeIcon(node).click({ modifiers: [CONTROL_KEY] })
  await expect(locate.componentBrowser(page)).toBeVisible()
  const input = locate.componentBrowserInput(page).locator('input')
  await expect(input).toHaveValue('Data.read')

  // Add argument and accept
  await page.keyboard.press('End')
  await input.pressSequentially(` ${ADDED_PATH}`)
  await expect(input).toHaveValue(`Data.read ${ADDED_PATH}`)
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(node.locator('.WidgetToken')).toHaveText(['Data', '.', 'read', '"', '"'])
  await expect(node.locator('.WidgetText input')).toHaveValue(ADDED_PATH.replaceAll('"', ''))

  // Edit again, using "edit" button
  await locate.graphNodeIcon(node).click()
  await node.getByTestId('edit-button').click()
  await expect(locate.componentBrowser(page)).toBeVisible()
  await expect(input).toHaveValue(`Data.read ${ADDED_PATH}`)
  for (let i = 0; i < ADDED_PATH.length; ++i) await page.keyboard.press('Backspace')
  await expect(input).toHaveValue('Data.read ')
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(node.locator('.WidgetToken')).toHaveText(['Data', '.', 'read'])
  await expect(node.locator('.WidgetText')).toBeHidden()
})

test('Visualization preview: type-based visualization selection', async ({ page }) => {
  await actions.goToGraph(page)
  const nodeCount = await locate.graphNode(page).count()
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toExist()
  await expect(locate.componentBrowserEntry(page)).toExist()
  const input = locate.componentBrowserInput(page).locator('input')
  await input.fill('Table.ne')
  await expect(input).toHaveValue('Table.ne')
  await locate.componentBrowser(page).getByTestId('switchToEditMode').click()
  await expect(locate.tableVisualization(page)).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount)
})

test('Visualization preview: user visualization selection', async ({ page }) => {
  await actions.goToGraph(page)
  const nodeCount = await locate.graphNode(page).count()
  await locate.addNewNodeButton(page).click()
  await expect(locate.componentBrowser(page)).toExist()
  const input = locate.componentBrowserInput(page).locator('input')
  await input.fill('4')
  await expect(input).toHaveValue('4')
  await locate.componentBrowser(page).getByTestId('switchToEditMode').click()
  await expect(locate.jsonVisualization(page)).toBeVisible()
  await expect(locate.jsonVisualization(page)).toContainText('"visualizedExpr": "4"')
  await locate.toggleVisualizationSelectorButton(page).click()
  await page.getByRole('button', { name: 'Table' }).click()
  await expect(locate.tableVisualization(page)).toBeVisible()
  await page.keyboard.press('Escape')
  await expect(locate.componentBrowser(page)).toBeHidden()
  await expect(locate.graphNode(page)).toHaveCount(nodeCount)
})

test('Component browser handling of overridden record-mode', async ({ page }) => {
  await actions.goToGraph(page)
  const node = locate.graphNodeByBinding(page, 'data')
  const ADDED_PATH = '"/home/enso/Input.txt"'
  const recordModeToggle = node.getByTestId('toggleRecord')
  const recordModeIndicator = node.getByTestId('recordingOverriddenButton')

  // Enable record mode for the node.
  await locate.graphNodeIcon(node).hover()
  await expect(recordModeToggle).toHaveClass(/toggledOff/)
  await recordModeToggle.click()
  await expect(recordModeToggle).toHaveClass(/toggledOn/)
  await page.keyboard.press('Escape')
  // TODO[ao]: The simple move near top-left corner not always works i.e. not always
  //  `pointerleave` event is emitted. Investigated in https://github.com/enso-org/enso/issues/9478
  //  once fixed, remember to change the second `await page.mouse.move(700, 1200, { steps: 20 })`
  //  line below.
  await page.mouse.move(700, 1200, { steps: 20 })
  await expect(recordModeIndicator).toBeVisible()
  await locate.graphNodeIcon(node).hover()
  await expect(recordModeToggle).toHaveClass(/toggledOn/)
  // Ensure editing in the component browser doesn't display the override expression.
  await locate.graphNodeIcon(node).click({ modifiers: [CONTROL_KEY] })
  await expect(locate.componentBrowser(page)).toBeVisible()
  const input = locate.componentBrowserInput(page).locator('input')
  await expect(input).toHaveValue('Data.read')
  // Ensure committing an edit doesn't change the override state.
  await page.keyboard.press('End')
  await input.pressSequentially(` ${ADDED_PATH}`)
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowser(page)).toBeHidden()
  // See TODO above.
  await page.mouse.move(700, 1200, { steps: 20 })
  await expect(recordModeIndicator).toBeVisible()
  // Ensure after editing the node, editing still doesn't display the override expression.
  await locate.graphNodeIcon(node).click({ modifiers: [CONTROL_KEY] })
  await expect(locate.componentBrowser(page)).toBeVisible()
  await expect(input).toHaveValue(`Data.read ${ADDED_PATH}`)
})

test('AI prompt', async ({ page }) => {
  await actions.goToGraph(page)

  const node = locate.graphNodeByBinding(page, 'data')
  await node.click()
  await expect(node).toBeSelected()
  await locate.graphEditor(page).press('Enter')
  await expect(locate.componentBrowser(page)).toBeVisible()

  await page.keyboard.insertText('AI:convert to table')
  await page.keyboard.press('Enter')
  await expect(locate.componentBrowserInput(page).locator('input')).toHaveValue('to_table')
  await expect(locate.componentBrowser(page)).toHaveAttribute('data-self-argument', 'data')
})
