import test, { type Locator, type Page } from 'playwright/test'
import * as actions from './actions'
import { expect } from './customExpect'
import { mockMethodCallInfo } from './expressionUpdates'
import * as locate from './locate'

class DropDownLocator {
  readonly rootWidget: Locator
  readonly dropDown: Locator
  readonly items: Locator
  readonly selectedItems: Locator

  constructor(ancestor: Locator) {
    this.rootWidget = ancestor.locator('.WidgetSelection')
    this.dropDown = ancestor.locator('.Dropdown')
    this.items = this.dropDown.locator('.item')
    this.selectedItems = this.dropDown.locator('.item.selected')
  }

  async expectVisibleWithOptions(options: string[]): Promise<void> {
    const page = this.dropDown.page()
    await expect(this.items.first()).toBeVisible()
    for (const option of options) {
      await expect(
        this.items.filter({ has: page.getByText(option, { exact: true }) }),
      ).toBeVisible()
    }
    await expect(this.items).toHaveCount(options.length)
  }

  async clickOption(option: string): Promise<void> {
    const page = this.dropDown.page()
    await this.items.filter({ has: page.getByText(option) }).click()
  }

  async openWithArrow(): Promise<void> {
    await this.rootWidget.hover()
    await expect(this.rootWidget.locator('.arrow')).toBeVisible()
    await this.rootWidget.locator('.arrow').click({ force: true })
    await expect(this.dropDown).toBeVisible()
  }
}

test('Widget in plain AST', async ({ page }) => {
  await actions.goToGraph(page)
  const numberNode = locate.graphNodeByBinding(page, 'five')
  const numberWidget = numberNode.locator('.WidgetNumber')
  await expect(numberWidget).toBeVisible()
  await expect(numberWidget.locator('input')).toHaveValue('5')

  const listNode = locate.graphNodeByBinding(page, 'list')
  const listWidget = listNode.locator('.WidgetVector')
  await expect(listWidget).toBeVisible()

  const textNode = locate.graphNodeByBinding(page, 'text')
  const textWidget = textNode.locator('.WidgetText')
  await expect(textWidget).toBeVisible()
  await expect(textWidget.locator('input')).toHaveValue('test')
})

test('Multi-selection widget', async ({ page }) => {
  await actions.goToGraph(page)
  await mockMethodCallInfo(page, 'selected', {
    methodPointer: {
      module: 'Standard.Table.Table',
      definedOnType: 'Standard.Table.Table.Table',
      name: 'select_columns',
    },
    notAppliedArguments: [1],
  })

  // Click the argument to open the dropdown.
  const node = locate.graphNodeByBinding(page, 'selected')
  const argumentNames = node.locator('.WidgetArgumentName')
  await expect(argumentNames).toHaveCount(1)
  await argumentNames.first().click()

  // Get the dropdown and corresponding vector; they both have 0 items.
  const dropDown = new DropDownLocator(node)
  await dropDown.expectVisibleWithOptions(['Column A', 'Column B'])
  await expect(dropDown.rootWidget).toHaveClass(/multiSelect/)
  const vector = node.locator('.WidgetVector')
  const vectorItems = vector.locator('.item .WidgetPort input')
  await expect(vector).toBeVisible()
  await expect(dropDown.selectedItems).toHaveCount(0)
  await expect(vectorItems).toHaveCount(0)

  // Enable an item.
  await dropDown.clickOption('Column A')
  await expect(vector).toBeVisible()
  await expect(vectorItems).toHaveCount(1)
  await expect(vectorItems.first()).toHaveValue('Column A')
  // Known bug: Dropdown closes after first item has been set.
  //await page.keyboard('Escape')

  // Add-item button opens dropdown.
  await vector.locator('.add-item').click()
  await expect(dropDown.items).toHaveCount(2)
  await expect(dropDown.selectedItems).toHaveCount(1)

  // Enable another item.
  await dropDown.clickOption('Column B')
  await expect(vectorItems).toHaveCount(2)
  await expect(vectorItems.first()).toHaveValue('Column A')
  await expect(vectorItems.nth(1)).toHaveValue('Column B')
  await expect(dropDown.dropDown).toBeVisible()
  await expect(dropDown.items).toHaveCount(2)
  await expect(dropDown.selectedItems).toHaveCount(2)

  // Disable an item.
  await dropDown.clickOption('Column A')
  await expect(vectorItems).toHaveCount(1)
  await expect(vectorItems.first()).toHaveValue('Column B')
  await expect(dropDown.dropDown).toBeVisible()
  await expect(dropDown.items).toHaveCount(2)
  await expect(dropDown.selectedItems).toHaveCount(1)

  // Disable the last item.
  await dropDown.clickOption('Column B')
  await expect(vectorItems).toHaveCount(0)
  await expect(dropDown.dropDown).toBeVisible()
  await expect(dropDown.items).toHaveCount(2)
  await expect(dropDown.selectedItems).toHaveCount(0)
})

async function dataReadNodeWithMethodCallInfo(page: Page): Promise<Locator> {
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1, 2],
  })
  return locate.graphNodeByBinding(page, 'data')
}

test('Selection widgets in Data.read node', async ({ page }) => {
  await actions.goToGraph(page)

  // Check initially visible arguments
  const node = await dataReadNodeWithMethodCallInfo(page)
  const dropDown = new DropDownLocator(node)
  const argumentNames = node.locator('.WidgetArgumentName')
  await expect(argumentNames).toHaveCount(1)

  // Check arguments after selecting node
  await node.click()
  await expect(argumentNames).toHaveCount(3)

  // Set value on `on_problems` (static drop-down)
  const onProblemsArg = argumentNames.filter({ has: page.getByText('on_problems') })
  await onProblemsArg.click()
  await dropDown.expectVisibleWithOptions(['Ignore', 'Report_Warning', 'Report_Error'])
  await dropDown.clickOption('Report_Error')
  await expect(onProblemsArg.locator('.WidgetToken')).toContainText([
    'Problem_Behavior',
    '.',
    'Report_Error',
  ])

  // Change value on `on_problems`
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1],
  })
  await page.getByText('Report_Error').click()
  await dropDown.expectVisibleWithOptions(['Ignore', 'Report_Warning', 'Report_Error'])
  await dropDown.clickOption('Report_Warning')
  await expect(onProblemsArg.locator('.WidgetToken')).toContainText([
    'Problem_Behavior',
    '.',
    'Report_Warning',
  ])

  // Set value on `path` (dynamic config)
  const pathArg = argumentNames.filter({ has: page.getByText('path') })
  await pathArg.click()
  await expect(page.locator('.Dropdown')).toBeVisible()
  await dropDown.expectVisibleWithOptions(['Choose file…', 'File 1', 'File 2'])
  await dropDown.clickOption('File 2')
  await expect(pathArg.locator('.WidgetText > input')).toHaveValue('File 2')

  // Change value on `path` (dynamic config)
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [1],
  })
  await page.getByText('path').click()
  await dropDown.expectVisibleWithOptions(['Choose file…', 'File 1', 'File 2'])
  await dropDown.clickOption('File 1')
  await expect(pathArg.locator('.WidgetText > input')).toHaveValue('File 1')
})

test('Selection widget with text widget as input', async ({ page }) => {
  await actions.goToGraph(page)

  const node = await dataReadNodeWithMethodCallInfo(page)
  const dropDown = new DropDownLocator(node)
  const argumentNames = node.locator('.WidgetArgumentName')
  const pathArg = argumentNames.filter({ has: page.getByText('path') })
  const pathArgInput = pathArg.locator('.WidgetText > input')
  await pathArg.click()
  await expect(page.locator('.Dropdown')).toBeVisible()
  await dropDown.clickOption('File 2')
  await expect(pathArgInput).toHaveValue('File 2')

  // Editing text input shows and filters drop down
  await pathArgInput.click()
  await dropDown.expectVisibleWithOptions(['Choose file…', 'File 1', 'File 2'])
  await page.keyboard.insertText('File 1')
  await dropDown.expectVisibleWithOptions(['File 1'])
  // Clearing input should show all text literal options
  await pathArgInput.clear()
  await dropDown.expectVisibleWithOptions(['File 1', 'File 2'])

  // Esc should cancel editing and close drop down
  await page.keyboard.press('Escape')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveValue('File 2')
  await expect(dropDown.dropDown).not.toBeVisible()

  // Choosing entry should finish editing
  await pathArgInput.click()
  await dropDown.expectVisibleWithOptions(['Choose file…', 'File 1', 'File 2'])
  await page.keyboard.insertText('File')
  await dropDown.expectVisibleWithOptions(['File 1', 'File 2'])
  await dropDown.clickOption('File 1')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveValue('File 1')
  await expect(dropDown.dropDown).not.toBeVisible()

  // Clicking-off and pressing enter should accept text as-is
  await pathArgInput.click()
  await dropDown.expectVisibleWithOptions(['Choose file…', 'File 1', 'File 2'])
  await page.keyboard.insertText('File')
  await page.keyboard.press('Enter')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveValue('File')
  await expect(dropDown.dropDown).not.toBeVisible()

  await pathArgInput.click()
  await dropDown.expectVisibleWithOptions(['Choose file…', 'File 1', 'File 2'])
  await page.keyboard.insertText('Foo')
  await expect(pathArgInput).toHaveValue('Foo')
  await page.mouse.click(200, 200)
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveValue('Foo')
  await expect(dropDown.dropDown).not.toBeVisible()
})

test('File Browser widget', async ({ page }) => {
  await actions.goToGraph(page)
  await mockMethodCallInfo(page, 'data', {
    methodPointer: {
      module: 'Standard.Base.Data',
      definedOnType: 'Standard.Base.Data',
      name: 'read',
    },
    notAppliedArguments: [0, 1, 2],
  })
  // Wait for arguments to load.
  const node = locate.graphNodeByBinding(page, 'data')
  const dropDown = new DropDownLocator(node)
  const argumentNames = node.locator('.WidgetArgumentName')
  await expect(argumentNames).toHaveCount(1)
  const pathArg = argumentNames.filter({ has: page.getByText('path') })

  await pathArg.click()
  await dropDown.expectVisibleWithOptions(['Choose file…', 'File 1', 'File 2'])
  await dropDown.clickOption('Choose file…')
  await expect(pathArg.locator('.WidgetText > input')).toHaveValue('/path/to/some/mock/file')
})

test('Managing aggregates in `aggregate` node', async ({ page }) => {
  await actions.goToGraph(page)
  await mockMethodCallInfo(page, 'aggregated', {
    methodPointer: {
      module: 'Standard.Table.Table',
      definedOnType: 'Standard.Table.Table.Table',
      name: 'aggregate',
    },
    notAppliedArguments: [1, 2, 3],
  })

  // Check initially visible arguments
  const node = locate.graphNodeByBinding(page, 'aggregated')
  const dropDown = new DropDownLocator(node)
  const argumentNames = node.locator('.WidgetArgumentName')
  await expect(argumentNames).toHaveCount(1)

  // Check arguments after selecting node
  await node.click()
  await expect(argumentNames).toHaveCount(3)

  // Add first aggregate
  const columnsArg = argumentNames.filter({ has: page.getByText('columns') })
  await columnsArg.locator('.add-item').click()
  await expect(columnsArg.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Group_By',
  ])
  await mockMethodCallInfo(
    page,
    {
      binding: 'aggregated',
      expr: 'Aggregate_Column.Group_By',
    },
    {
      methodPointer: {
        module: 'Standard.Table.Aggregate_Column',
        definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
        name: 'Group_By',
      },
      notAppliedArguments: [0, 1],
    },
  )

  // Change aggregation type
  const firstItem = columnsArg.locator('.item > .WidgetPort > .WidgetSelection')
  await firstItem.click()
  await dropDown.expectVisibleWithOptions(['Group_By', 'Count', 'Count_Distinct'])
  await dropDown.clickOption('Count_Distinct')
  await expect(columnsArg.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Count_Distinct',
  ])
  await mockMethodCallInfo(
    page,
    {
      binding: 'aggregated',
      expr: 'Aggregate_Column.Count_Distinct',
    },
    {
      methodPointer: {
        module: 'Standard.Table.Aggregate_Column',
        definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
        name: 'Count_Distinct',
      },
      notAppliedArguments: [0, 1, 2],
    },
  )

  // Set column
  const columnArg = firstItem.locator('.WidgetSelection').first()
  await columnArg.click()
  await dropDown.expectVisibleWithOptions(['column 1', 'column 2'])
  await dropDown.clickOption('column 1')
  await expect(columnsArg.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Count_Distinct',
  ])
  await expect(columnsArg.locator('.WidgetText > input').first()).toHaveValue('column 1')

  // Add another aggregate
  await columnsArg.locator('.add-item').click()
  await expect(columnsArg.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Count_Distinct',
    'Aggregate_Column',
    '.',
    'Group_By',
  ])
  await mockMethodCallInfo(
    page,
    {
      binding: 'aggregated',
      expr: 'Aggregate_Column.Group_By',
    },
    {
      methodPointer: {
        module: 'Standard.Table.Aggregate_Column',
        definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
        name: 'Group_By',
      },
      notAppliedArguments: [0, 1],
    },
  )

  // Set new aggregate's column
  const secondItem = columnsArg.locator('.item > .WidgetPort > .WidgetSelection').nth(1)
  const secondColumnArg = secondItem.locator('.WidgetSelection').first()
  await secondColumnArg.click()
  await dropDown.expectVisibleWithOptions(['column 1', 'column 2'])
  await dropDown.clickOption('column 2')
  await expect(secondItem.locator('.WidgetToken')).toContainText([
    'Aggregate_Column',
    '.',
    'Group_By',
  ])
  await expect(secondItem.locator('.WidgetText > input').first()).toHaveValue('column 2')

  // Switch aggregates
  //TODO[ao] I have no idea how to emulate drag. Simple dragTo does not work (some element seem to capture event).
  // When hovered, the handle becomes available after some time, but still mouse events don't have any effect.
  // I have no time now to investigate this.
  // Once fixed, add also removing element from vector here.

  // await columnsArg.locator('.item > .handle').nth(1).hover({ force: true })
  // await columnsArg.locator('.item > .handle').nth(1).hover()
  // await page.mouse.down()
  // await columnsArg.locator('.item > .handle').nth(0).hover({ force: true })
  // await columnsArg.locator('.item > .handle').nth(0).hover()
  // await page.mouse.up()
  // await expect(columnsArg.locator('.WidgetToken')).toContainText([
  //   'Aggregate_Column',
  //   '.',
  //   'Group_By',
  //   '"',
  //   'column 2',
  //   '"',
  //   'Aggregate_Column',
  //   '.',
  //   'Count_Distinct',
  //   '"',
  //   'column 1',
  //   '"',
  // ])
})
