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
    this.rootWidget = ancestor.locator('.WidgetSelection').first()
    const page = ancestor.page()
    // There can be only one open dropdown at a time on a page. We have to filter out the ones that
    // still have leaving animation running.
    this.dropDown = page.locator('.DropdownWidget:not([data-transitioning])')
    this.items = this.dropDown.locator('.item')
    this.selectedItems = this.dropDown.locator('.item.selected')
  }

  async expectVisibleWithOptions(options: string[]): Promise<void> {
    await this.expectVisible()
    const page = this.dropDown.page()
    await expect(this.items.first()).toBeVisible()
    for (const option of options) {
      await expect(
        this.items.filter({ has: page.getByText(option, { exact: true }) }),
      ).toBeVisible()
    }
    await expect(this.items).toHaveCount(options.length)
  }

  async expectVisible(): Promise<void> {
    await expect(this.dropDown).toHaveCount(1)
    await expect(this.dropDown).toBeVisible()
  }

  async expectNotVisible(): Promise<void> {
    await expect(this.dropDown).toBeHidden()
  }

  async clickOption(option: string): Promise<void> {
    const item = this.item(option)
    await item.click()
  }

  async clickWidget(): Promise<void> {
    await this.rootWidget.click()
  }

  selectedItem(text: string): Locator {
    const page = this.dropDown.page()
    return this.selectedItems.filter({ has: page.getByText(text) })
  }

  item(text: string): Locator {
    const page = this.dropDown.page()
    return this.items.filter({ has: page.getByText(text) })
  }
}

const CHOOSE_CLOUD_FILE = 'Choose file from cloud...'
const CHOOSE_LOCAL_FILE = 'Choose file…'
const CHOOSE_FILE_OPTIONS = [CHOOSE_CLOUD_FILE, CHOOSE_LOCAL_FILE]

test('Widget in plain AST', async ({ page }) => {
  await actions.goToGraph(page)
  const numberNode = locate.graphNodeByBinding(page, 'five')
  const numberWidget = numberNode.locator('.WidgetNumber')
  await expect(numberWidget).toBeVisible()
  await expect(numberWidget).toHaveValue('5')

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
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  await expect(topLevelArgs).toHaveCount(1)
  const columnsArg = topLevelArgs.filter({ has: page.getByText('columns') })

  // Get the dropdown and corresponding vector; they both have 0 items.
  const dropDown = new DropDownLocator(columnsArg)
  await dropDown.clickWidget()
  await dropDown.expectVisibleWithOptions(['Column A', 'Column B'])
  await expect(dropDown.rootWidget).toHaveClass(/multiSelect/)
  const vector = node.locator('.WidgetVector')
  const vectorItems = vector.locator('.item .WidgetPort input')
  await expect(vector).toBeVisible()
  await expect(dropDown.selectedItems).toHaveCount(0)
  await expect(vectorItems).toHaveCount(0)

  // Enable an item.
  await dropDown.clickOption('Column A')
  await expect(dropDown.selectedItem('Column A')).toExist()
  await expect(vector).toBeVisible()
  await expect(vectorItems).toHaveCount(1)
  await expect(vectorItems.first()).toHaveValue('Column A')
  await dropDown.expectVisibleWithOptions(['Column A', 'Column B'])

  // Add-item button opens dropdown, after closing with escape.
  await page.keyboard.press('Escape')
  await dropDown.expectNotVisible()
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

  // Clicking to edit an item opens the dropdown, after closing with escape.
  await page.keyboard.press('Escape')
  await dropDown.expectNotVisible()
  await expect(vectorItems.first()).toHaveValue('Column A')
  await vectorItems.first().click()
  await expect(vectorItems.first()).toBeFocused()
  await expect(dropDown.dropDown).toBeVisible()

  // Clicking to edit a different item doesn't close the dropdown.
  await expect(vectorItems.nth(1)).toHaveValue('Column B')
  await vectorItems.nth(1).click()
  await expect(vectorItems.nth(1)).toBeFocused()
  await expect(dropDown.dropDown).toBeVisible()

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

test('Multi-selection widget: Item edits', async ({ page }) => {
  await actions.goToGraph(page)
  await mockMethodCallInfo(page, 'selected', {
    methodPointer: {
      module: 'Standard.Table.Table',
      definedOnType: 'Standard.Table.Table.Table',
      name: 'select_columns',
    },
    notAppliedArguments: [1],
  })

  // Get the dropdown and set it up by enabling two items.
  const columnsArg = locate
    .graphNodeByBinding(page, 'selected')
    .locator('.WidgetTopLevelArgument')
    .filter({ has: page.getByText('columns') })
  const vectorItems = columnsArg.locator('.WidgetVector .item .WidgetPort input')
  const dropDown = new DropDownLocator(columnsArg)
  await dropDown.clickWidget()
  await dropDown.clickOption('Column A')
  await dropDown.clickOption('Column B')

  // Edit an item
  await expect(dropDown.selectedItem('Column A')).toExist()
  await expect(dropDown.selectedItem('Column B')).toExist()
  await expect(vectorItems.first()).toHaveValue('Column A')
  await vectorItems.first().fill('Something Else')
  await expect(dropDown.selectedItem('Column A')).toBeHidden()
  await expect(dropDown.selectedItem('Column B')).toExist()
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
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  await expect(topLevelArgs).toHaveCount(1)

  // Check arguments after selecting node
  await node.click()
  await expect(topLevelArgs).toHaveCount(3)

  // Set value on `on_problems` (static drop-down)
  const onProblemsArg = topLevelArgs.filter({ has: page.getByText('on_problems') })
  await onProblemsArg.click()
  const onProblemsDropdown = new DropDownLocator(onProblemsArg)
  await onProblemsDropdown.expectVisibleWithOptions(['Ignore', 'Report_Warning', 'Report_Error'])
  await onProblemsDropdown.clickOption('Report_Error')
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
  await onProblemsDropdown.expectVisibleWithOptions(['Ignore', 'Report_Warning', 'Report_Error'])
  await onProblemsDropdown.clickOption('Report_Warning')
  await expect(onProblemsArg.locator('.WidgetToken')).toContainText([
    'Problem_Behavior',
    '.',
    'Report_Warning',
  ])

  // Set value on `path` (dynamic config)
  const pathArg = topLevelArgs.filter({ has: page.getByText('path') })
  await pathArg.click()
  const pathDropdown = new DropDownLocator(pathArg)
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await pathDropdown.clickOption('File 2')
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
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await pathDropdown.clickOption('File 1')
  await expect(pathArg.locator('.WidgetText > input')).toHaveValue('File 1')
})

test('Selection widget with text widget as input', async ({ page }) => {
  await actions.goToGraph(page)

  const node = await dataReadNodeWithMethodCallInfo(page)
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  const pathArg = topLevelArgs.filter({ has: page.getByText('path') })
  const pathDropdown = new DropDownLocator(pathArg)
  const pathArgInput = pathArg.locator('.WidgetText > input')
  await pathArg.click()
  await pathDropdown.expectVisible()
  await pathDropdown.clickOption('File 2')
  await expect(pathArgInput).toHaveValue('File 2')

  // Editing text input shows and filters drop down
  await pathArgInput.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await page.keyboard.insertText('File 1')
  await pathDropdown.expectVisibleWithOptions(['File 1'])
  // Clearing input should show all text literal options
  await pathArgInput.clear()
  await pathDropdown.expectVisibleWithOptions(['File 1', 'File 2'])

  // Esc should cancel editing and close drop down
  await page.keyboard.press('Escape')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveValue('File 2')
  await expect(pathDropdown.dropDown).not.toBeVisible()

  // Choosing entry should finish editing
  await pathArgInput.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await page.keyboard.insertText('File')
  await pathDropdown.expectVisibleWithOptions(['File 1', 'File 2'])
  await pathDropdown.clickOption('File 1')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveValue('File 1')
  await expect(pathDropdown.dropDown).not.toBeVisible()

  // Clicking-off and pressing enter should accept text as-is
  await pathArgInput.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await page.keyboard.insertText('File')
  await page.keyboard.press('Enter')
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveValue('File')
  await expect(pathDropdown.dropDown).not.toBeVisible()

  await pathArgInput.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await page.keyboard.insertText('Foo')
  await expect(pathArgInput).toHaveValue('Foo')
  await actions.clickAtBackground(page)
  await expect(pathArgInput).not.toBeFocused()
  await expect(pathArgInput).toHaveValue('Foo')
  await expect(pathDropdown.dropDown).not.toBeVisible()
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
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  await expect(topLevelArgs).toHaveCount(1)
  const pathArg = topLevelArgs.filter({ has: page.getByText('path') })
  const pathDropdown = new DropDownLocator(pathArg)
  await pathArg.click()
  await pathDropdown.expectVisibleWithOptions([...CHOOSE_FILE_OPTIONS, 'File 1', 'File 2'])
  await pathDropdown.clickOption(CHOOSE_LOCAL_FILE)
  await expect(pathArg.locator('.WidgetText > input')).toHaveValue('/path/to/some/mock/file')
})

test('Manage aggregates in `aggregate` node', async ({ page }) => {
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
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  await expect(topLevelArgs).toHaveCount(1)

  // Check arguments after selecting node
  await node.click()
  await expect(topLevelArgs).toHaveCount(3)

  // Add first aggregate
  const columnsArg = topLevelArgs.filter({ has: page.getByText('columns') })

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
  const columnsDropdown = new DropDownLocator(columnsArg)
  await columnsDropdown.clickWidget()
  await columnsDropdown.expectVisibleWithOptions(['Group_By', 'Count', 'Count_Distinct'])
  await columnsDropdown.clickOption('Count_Distinct')
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
  const firstItem = columnsArg.locator('.item > .WidgetPort > .WidgetSelection').nth(0)
  const firstItemDropdown = new DropDownLocator(firstItem)
  await firstItemDropdown.clickWidget()
  await firstItemDropdown.expectVisibleWithOptions(['column 1', 'column 2'])
  await firstItemDropdown.clickOption('column 1')
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
  const secondItemDropdown = new DropDownLocator(secondItem)
  await secondItemDropdown.clickWidget()
  await secondItemDropdown.expectVisibleWithOptions(['column 1', 'column 2'])
  await secondItemDropdown.clickOption('column 2')
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

// Test that autoscoped constructors provide argument placeholders.
// This test can be removed when `aggregate` inserts autoscoped constructors by default,
// so this behavior will be tested in regular `aggregate` tests.
test('Autoscoped constructors', async ({ page }) => {
  await actions.goToGraph(page)
  await mockMethodCallInfo(page, 'autoscoped', {
    methodPointer: {
      module: 'Standard.Table.Table',
      definedOnType: 'Standard.Table.Table.Table',
      name: 'aggregate',
    },
    notAppliedArguments: [2, 3],
  })
  await mockMethodCallInfo(
    page,
    { binding: 'autoscoped', expr: '..Group_By' },
    {
      methodPointer: {
        module: 'Standard.Table.Aggregate_Column',
        definedOnType: 'Standard.Table.Aggregate_Column.Aggregate_Column',
        name: 'Group_By',
      },
      notAppliedArguments: [0, 1],
    },
  )
  const node = locate.graphNodeByBinding(page, 'autoscoped')
  const topLevelArgs = node.locator('.WidgetTopLevelArgument')
  // Wait for hidden arguments to appear after selecting the node.
  await node.click()
  await expect(topLevelArgs).toHaveCount(3)

  const groupBy = node.locator('.item').nth(0)
  await expect(groupBy).toBeVisible()
  await expect(groupBy.locator('.WidgetArgumentName')).toContainText(['column', 'new_name'])
})

test('Table widget', async ({ page }) => {
  await actions.goToGraph(page)

  const node = await actions.createTableNode(page)
  const widget = node.locator('.WidgetTableEditor')
  await expect(widget).toBeVisible()
  await expect(widget.locator('.ag-header-cell-text')).toHaveText(['#'])
  await expect(widget.getByRole('button', { name: 'Add new column' })).toExist()
  await expect(widget.locator('.ag-cell')).toHaveText(['0', ''])

  // Create first column
  await widget.getByRole('button', { name: 'Add new column' }).click()
  await expect(widget.locator('.ag-header-cell-text')).toHaveText(['#', 'Column #1'])
  await expect(widget.locator('.ag-cell')).toHaveText(['0', '', ''])

  // Putting first value
  await widget.locator('.ag-cell', { hasNotText: '0' }).first().click()
  await page.keyboard.type('Value')
  await page.keyboard.press('Enter')
  // There will be new blank row allowing adding new rows.
  await expect(widget.locator('.ag-cell')).toHaveText(['0', 'Value', '', '1', '', ''])

  // Renaming column
  await widget.locator('.ag-header-cell-text', { hasText: 'Column #1' }).first().click()
  await page.keyboard.type('Header')
  await page.keyboard.press('Enter')
  await expect(widget.locator('.ag-header-cell-text')).toHaveText(['#', 'Header'])

  // Adding next column
  await widget.getByRole('button', { name: 'Add new column' }).click()
  await expect(widget.locator('.ag-header-cell-text')).toHaveText(['#', 'Header', 'Column #2'])
  await expect(widget.locator('.ag-cell')).toHaveText(['0', 'Value', '', '', '1', '', '', ''])

  // Switching edit between cells and headers - check we will never edit two things at once.
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(0)
  await widget.locator('.ag-header-cell-text', { hasNotText: /#/ }).first().click()
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(1)
  await widget.locator('.ag-cell', { hasNotText: /0|1/ }).first().dblclick()
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(1)
  await widget.locator('.ag-header-cell-text', { hasNotText: /#/ }).first().click()
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(1)
  // The header after click stops editing immediately. Tracked by #11150
  // await widget.locator('.ag-header-cell-text', { hasNotText: /#/ }).last().dblclick()
  // await expect(widget.locator('.ag-text-field-input')).toHaveCount(1)
  await page.keyboard.press('Escape')
  await expect(widget.locator('.ag-text-field-input')).toHaveCount(0)
})
