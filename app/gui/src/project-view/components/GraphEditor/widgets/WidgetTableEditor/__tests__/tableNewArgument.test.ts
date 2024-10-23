import {
  DEFAULT_COLUMN_PREFIX,
  NEW_COLUMN_ID,
  ROW_INDEX_HEADER,
  RowData,
  tableNewCallMayBeHandled,
  useTableNewArgument,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableNewArgument'
import { MenuItem } from '@/components/shared/AgGridTableView.vue'
import { WidgetInput } from '@/providers/widgetRegistry'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import { makeType } from '@/stores/suggestionDatabase/entry'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { GetContextMenuItems, GetMainMenuItems } from 'ag-grid-enterprise'
import { expect, test, vi } from 'vitest'

function suggestionDbWithNothing() {
  const db = new SuggestionDb()
  db.set(1, makeType('Standard.Base.Nothing.Nothing'))
  return db
}

const expectedRowIndexColumnDef = { headerName: ROW_INDEX_HEADER }
const expectedNewColumnDef = { cellStyle: { display: 'none' } }

test.each([
  {
    code: 'Table.new [["a", [1, 2, 3]], ["b", [4, 5, "six"]], ["empty", [Nothing, Standard.Base.Nothing, Nothing]]]',
    expectedColumnDefs: [
      expectedRowIndexColumnDef,
      { headerName: 'a' },
      { headerName: 'b' },
      { headerName: 'empty' },
      expectedNewColumnDef,
    ],
    expectedRows: [
      { [ROW_INDEX_HEADER]: 0, a: 1, b: 4, empty: null, '': null },
      { [ROW_INDEX_HEADER]: 1, a: 2, b: 5, empty: null, '': null },
      { [ROW_INDEX_HEADER]: 2, a: 3, b: 'six', empty: null, '': null },
      { [ROW_INDEX_HEADER]: 3, a: null, b: null, empty: null, '': null },
    ],
  },
  {
    code: 'Table.new []',
    expectedColumnDefs: [expectedRowIndexColumnDef, expectedNewColumnDef],
    expectedRows: [{ [ROW_INDEX_HEADER]: 0, '': null }],
  },
  {
    code: 'Table.new',
    expectedColumnDefs: [expectedRowIndexColumnDef, expectedNewColumnDef],
    expectedRows: [{ [ROW_INDEX_HEADER]: 0, '': null }],
  },
  {
    code: 'Table.new _',
    expectedColumnDefs: [expectedRowIndexColumnDef, expectedNewColumnDef],
    expectedRows: [{ [ROW_INDEX_HEADER]: 0, '': null }],
  },
  {
    code: 'Table.new [["a", []]]',
    expectedColumnDefs: [expectedRowIndexColumnDef, { headerName: 'a' }, expectedNewColumnDef],
    expectedRows: [{ [ROW_INDEX_HEADER]: 0, a: null, '': null }],
  },
  {
    code: 'Table.new [["a", [1,,2]], ["b", [3, 4,]], ["c", [, 5, 6]], ["d", [,,]]]',
    expectedColumnDefs: [
      expectedRowIndexColumnDef,
      { headerName: 'a' },
      { headerName: 'b' },
      { headerName: 'c' },
      { headerName: 'd' },
      expectedNewColumnDef,
    ],
    expectedRows: [
      { [ROW_INDEX_HEADER]: 0, a: 1, b: 3, c: null, d: null, '': null },
      { [ROW_INDEX_HEADER]: 1, a: null, b: 4, c: 5, d: null, '': null },
      { [ROW_INDEX_HEADER]: 2, a: 2, b: null, c: 6, d: null, '': null },
      { [ROW_INDEX_HEADER]: 3, a: null, b: null, c: null, d: null, '': null },
    ],
  },
])('Read table from $code', ({ code, expectedColumnDefs, expectedRows }) => {
  const ast = Ast.parse(code)
  expect(tableNewCallMayBeHandled(ast)).toBeTruthy()
  const input = WidgetInput.FromAst(ast)
  const startEdit = vi.fn()
  const addMissingImports = vi.fn()
  const onUpdate = vi.fn()
  const tableNewArgs = useTableNewArgument(
    input,
    { startEdit, addMissingImports },
    suggestionDbWithNothing(),
    onUpdate,
  )
  expect(tableNewArgs.columnDefs.value).toEqual(
    Array.from(expectedColumnDefs, (colDef) => expect.objectContaining(colDef)),
  )
  const resolvedRow = Array.from(tableNewArgs.rowData.value, (row) =>
    Object.fromEntries(
      tableNewArgs.columnDefs.value.map((col) => [col.headerName, col.valueGetter({ data: row })]),
    ),
  )
  expect(resolvedRow).toEqual(expectedRows)

  function* expectedIndices() {
    for (let i = 0; i < expectedRows.length; ++i) {
      yield expect.objectContaining({ index: i })
    }
  }
  expect(tableNewArgs.rowData.value).toEqual([...expectedIndices()])
  expect(startEdit).not.toHaveBeenCalled()
  expect(onUpdate).not.toHaveBeenCalled()
  expect(addMissingImports).not.toHaveBeenCalled()
})

test.each([
  'Table.new 14',
  'Table.new array1',
  "Table.new ['a', [123]]",
  "Table.new [['a', [123]], ['b', [124], []]]",
  "Table.new [['a', [123]], ['a'.repeat 170, [123]]]",
  "Table.new [['a', [1, 2, 3, 3 + 1]]]",
])('"%s" is not valid input for Table Editor Widget', (code) => {
  const ast = Ast.parse(code)
  expect(tableNewCallMayBeHandled(ast)).toBeFalsy()
})

function tableEditFixture(code: string, expectedCode: string) {
  const ast = Ast.parseBlock(code)
  const inputAst = [...ast.statements()][0]
  assert(inputAst != null)
  const input = WidgetInput.FromAst(inputAst)
  const startEdit = vi.fn(() => ast.module.edit())
  const onUpdate = vi.fn((update) => {
    const inputAst = [...update.edit.getVersion(ast).statements()][0]
    expect(inputAst?.code()).toBe(expectedCode)
  })
  const addMissingImports = vi.fn((_, imports) => {
    // the only import we're going to add is Nothing.
    expect(imports).toEqual([
      {
        kind: 'Unqualified',
        from: 'Standard.Base.Nothing',
        import: 'Nothing',
      },
    ])
  })
  const tableNewArgs = useTableNewArgument(
    input,
    { startEdit, addMissingImports },
    suggestionDbWithNothing(),
    onUpdate,
  )
  const gridApi = {
    cutToClipboard: vi.fn(),
    copyToClipboard: vi.fn(),
    pasteFromClipboard: vi.fn(),
  }
  return { tableNewArgs, startEdit, onUpdate, addMissingImports, gridApi }
}

test.each([
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Edit number',
    edit: { column: 1, row: 1, value: -22 },
    expected: "Table.new [['a', [1, -22, 3]], ['b', [4, 5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Edit string',
    edit: { column: 1, row: 1, value: 'two' },
    expected: "Table.new [['a', [1, 'two', 3]], ['b', [4, 5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Put blank value',
    edit: { column: 2, row: 1, value: '' },
    expected: "Table.new [['a', [1, 2, 3]], ['b', [4, Nothing, 6]]]",
    importExpected: true,
  },

  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    description: 'Add new row',
    edit: { column: 1, row: 3, value: 4.5 },
    expected: "Table.new [['a', [1, 2, 3, 4.5]], ['b', [4, 5, 6, Nothing]]]",
    importExpected: true,
  },
  {
    code: "Table.new [['a', []], ['b', []]]",
    description: 'Add first row',
    edit: { column: 2, row: 0, value: 'val' },
    expected: "Table.new [['a', [Nothing]], ['b', ['val']]]",
    importExpected: true,
  },
  {
    code: "Table.new [['a', [1, ,3]]]",
    description: 'Set missing value',
    edit: { column: 1, row: 1, value: 2 },
    expected: "Table.new [['a', [1, 2 ,3]]]",
  },
  {
    code: "Table.new [['a', [, 2, 3]]]",
    description: 'Set missing value at first row',
    edit: { column: 1, row: 0, value: 1 },
    expected: "Table.new [['a', [1, 2, 3]]]",
  },
  {
    code: "Table.new [['a', [1, 2,]]]",
    description: 'Set missing value at last row',
    edit: { column: 1, row: 2, value: 3 },
    expected: "Table.new [['a', [1, 2, 3]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['a', [3, 4]]]",
    description: 'Edit with duplicated column name',
    edit: { column: 1, row: 1, value: 5 },
    expected: "Table.new [['a', [1, 5]], ['a', [3, 4]]]",
  },
])('Edit table $code: $description', ({ code, edit, expected, importExpected }) => {
  const { tableNewArgs, onUpdate, addMissingImports } = tableEditFixture(code, expected)
  const editedRow = tableNewArgs.rowData.value[edit.row]
  assert(editedRow != null)
  tableNewArgs.columnDefs.value[edit.column]?.valueSetter?.({
    data: editedRow,
    newValue: edit.value,
  })
  expect(onUpdate).toHaveBeenCalledOnce()
  if (importExpected) expect(addMissingImports).toHaveBeenCalled()
  else expect(addMissingImports).not.toHaveBeenCalled()
})

test.each([
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    expected: `Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]], ['${DEFAULT_COLUMN_PREFIX}3', [Nothing, Nothing, Nothing]]]`,
    importExpected: true,
  },
  {
    code: 'Table.new []',
    expected: `Table.new [['${DEFAULT_COLUMN_PREFIX}1', []]]`,
  },
  {
    code: 'Table.new',
    expected: `Table.new [['${DEFAULT_COLUMN_PREFIX}1', []]]`,
  },
  {
    code: 'Table.new _',
    expected: `Table.new [['${DEFAULT_COLUMN_PREFIX}1', []]]`,
  },
])('Add column to table $code', ({ code, expected, importExpected }) => {
  const { tableNewArgs, onUpdate, addMissingImports } = tableEditFixture(code, expected)
  const newColumnDef = tableNewArgs.columnDefs.value.find(
    (colDef) => colDef.colId === NEW_COLUMN_ID,
  )
  assert(newColumnDef != null)
  assert(newColumnDef.headerComponentParams?.type === 'newColumn')
  assert(newColumnDef.headerComponentParams.newColumnRequested != null)
  newColumnDef.headerComponentParams.newColumnRequested()
  expect(onUpdate).toHaveBeenCalledOnce()
  if (importExpected) expect(addMissingImports).toHaveBeenCalled()
  else expect(addMissingImports).not.toHaveBeenCalled()
})

function getCustomMenuItemByName(
  name: string,
  items:
    | (string | MenuItem<RowData>)[]
    | GetMainMenuItems<RowData>
    | GetContextMenuItems<RowData>
    | undefined,
): MenuItem<RowData> | undefined {
  if (!(items instanceof Array)) return undefined
  const found = items.find((item) => typeof item === 'object' && item.name === name)
  return typeof found === 'object' ? found : undefined
}

test.each([
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    removedRowIndex: 0,
    expected: "Table.new [['a', [2, 3]], ['b', [5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    removedRowIndex: 1,
    expected: "Table.new [['a', [1, 3]], ['b', [4, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    removedRowIndex: 2,
    expected: "Table.new [['a', [1, 2]], ['b', [4, 5]]]",
  },
  {
    code: "Table.new [['a', [1]], ['b', [4]]]",
    removedRowIndex: 0,
    expected: "Table.new [['a', []], ['b', []]]",
  },
])('Remove $removedRowIndex row in $code', ({ code, removedRowIndex, expected }) => {
  const { tableNewArgs, onUpdate, addMissingImports, gridApi } = tableEditFixture(code, expected)
  const removedRow = tableNewArgs.rowData.value[removedRowIndex]
  assert(removedRow != null)
  // Context menu of all cells in given row should work (even the "virtual" columns).
  for (const colDef of tableNewArgs.columnDefs.value) {
    const removeAction = getCustomMenuItemByName('Remove Row', colDef.contextMenuItems)
    assert(removeAction != null)
    removeAction.action({ node: { data: removedRow }, api: gridApi })
    expect(onUpdate).toHaveBeenCalledOnce()
    onUpdate.mockClear()
  }
  expect(addMissingImports).not.toHaveBeenCalled()
})

test.each([
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]], ['c', [5, 6]]]",
    removedColIndex: 1,
    expected: "Table.new [['b', [3, 4]], ['c', [5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]], ['c', [5, 6]]]",
    removedColIndex: 2,
    expected: "Table.new [['a', [1, 2]], ['c', [5, 6]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]], ['c', [5, 6]]]",
    removedColIndex: 3,
    expected: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
  },
  {
    code: "Table.new [['a', [1, 2]]]",
    removedColIndex: 1,
    expected: 'Table.new []',
  },
])('Remove $removedColIndex column in $code', ({ code, removedColIndex, expected }) => {
  const { tableNewArgs, onUpdate, addMissingImports, gridApi } = tableEditFixture(code, expected)
  const removedCol = tableNewArgs.columnDefs.value[removedColIndex]
  assert(removedCol != null)
  const removeAction = getCustomMenuItemByName('Remove Column', removedCol.mainMenuItems)
  assert(removeAction != null)
  removeAction.action({ node: null, api: gridApi })
  expect(onUpdate).toHaveBeenCalledOnce()
  onUpdate.mockClear()

  const cellRemoveAction = getCustomMenuItemByName('Remove Column', removedCol.contextMenuItems)
  cellRemoveAction?.action({ node: { data: tableNewArgs.rowData.value[0] }, api: gridApi })
  expect(onUpdate).toHaveBeenCalledOnce()

  expect(addMissingImports).not.toHaveBeenCalled()
})

test.each([
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]], ['c', [5, 6]]]",
    fromIndex: 1,
    toIndex: 3,
    expected: "Table.new [['b', [3, 4]], ['c', [5, 6]], ['a', [1, 2]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]], ['c', [5, 6]]]",
    fromIndex: 3,
    toIndex: 2,
    expected: "Table.new [['a', [1, 2]], ['c', [5, 6]], ['b', [3, 4]]]",
  },
])(
  'Move column $fromIndex to $toIndex in table $code',
  ({ code, fromIndex, toIndex, expected }) => {
    const { tableNewArgs, onUpdate, addMissingImports } = tableEditFixture(code, expected)
    const movedColumnDef = tableNewArgs.columnDefs.value[fromIndex]
    assert(movedColumnDef?.colId != null)
    tableNewArgs.moveColumn(movedColumnDef.colId, toIndex)
    expect(onUpdate).toHaveBeenCalledOnce()
    expect(addMissingImports).not.toHaveBeenCalled()
  },
)

test.each([
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    fromIndex: 1,
    toIndex: 2,
    expected: "Table.new [['a', [1, 3, 2]], ['b', [4, 6, 5]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    fromIndex: 2,
    toIndex: 0,
    expected: "Table.new [['a', [3, 1, 2]], ['b', [6, 4, 5]]]",
  },
  {
    code: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
    fromIndex: 1,
    toIndex: -1,
    expected: "Table.new [['a', [1, 2, 3]], ['b', [4, 5, 6]]]",
  },
])('Move row $fromIndex to $toIndex in table $code', ({ code, fromIndex, toIndex, expected }) => {
  const { tableNewArgs, onUpdate, addMissingImports } = tableEditFixture(code, expected)
  tableNewArgs.moveRow(fromIndex, toIndex)
  if (code !== expected) {
    expect(onUpdate).toHaveBeenCalledOnce()
  }
  expect(addMissingImports).not.toHaveBeenCalled()
})

test.each([
  {
    code: 'Table.new',
    focused: { rowIndex: 0, colIndex: 1 },
    data: [
      ['1', '3'],
      ['2', '4'],
    ],
    expected: `Table.new [['${DEFAULT_COLUMN_PREFIX}1', [1, 2]], ['${DEFAULT_COLUMN_PREFIX}2', [3, 4]]]`,
  },
  {
    code: 'Table.new []',
    focused: { rowIndex: 0, colIndex: 1 },
    data: [
      ['1', '3'],
      ['2', '4'],
    ],
    expected: `Table.new [['${DEFAULT_COLUMN_PREFIX}1', [1, 2]], ['${DEFAULT_COLUMN_PREFIX}2', [3, 4]]]`,
  },
  {
    code: 'Table.new []',
    focused: { rowIndex: 0, colIndex: 1 },
    data: [['a single cell']],
    expected: `Table.new [['${DEFAULT_COLUMN_PREFIX}1', ['a single cell']]]`,
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 0, colIndex: 1 },
    data: [['a single cell']],
    expected: "Table.new [['a', ['a single cell', 2]], ['b', [3, 4]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 1, colIndex: 2 },
    data: [['a single cell']],
    expected: "Table.new [['a', [1, 2]], ['b', [3, 'a single cell']]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 2, colIndex: 2 },
    data: [['a single cell']],
    expected: "Table.new [['a', [1, 2, Nothing]], ['b', [3, 4, 'a single cell']]]",
    importExpected: true,
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 1, colIndex: 3 },
    data: [['a single cell']],
    expected: `Table.new [['a', [1, 2]], ['b', [3, 4]], ['${DEFAULT_COLUMN_PREFIX}3', [Nothing, 'a single cell']]]`,
    importExpected: true,
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 0, colIndex: 1 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: "Table.new [['a', [5, 6]], ['b', [7, 8]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 1, colIndex: 1 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: "Table.new [['a', [1, 5, 6]], ['b', [3, 7, 8]]]",
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 0, colIndex: 2 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: `Table.new [['a', [1, 2]], ['b', [5, 6]], ['${DEFAULT_COLUMN_PREFIX}3', [7, 8]]]`,
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 1, colIndex: 2 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: `Table.new [['a', [1, 2, Nothing]], ['b', [3, 5, 6]], ['${DEFAULT_COLUMN_PREFIX}3', [Nothing, 7, 8]]]`,
    importExpected: true,
  },
  {
    code: "Table.new [['a', [1, 2]], ['b', [3, 4]]]",
    focused: { rowIndex: 2, colIndex: 2 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: `Table.new [['a', [1, 2, Nothing, Nothing]], ['b', [3, 4, 5, 6]], ['${DEFAULT_COLUMN_PREFIX}3', [Nothing, Nothing, 7, 8]]]`,
    importExpected: true,
  },
])(
  'Paste data $data to table $code at $focused',
  ({ code, focused, data, expected, importExpected }) => {
    const { tableNewArgs, onUpdate, addMissingImports } = tableEditFixture(code, expected)
    const focusedCol = tableNewArgs.columnDefs.value[focused.colIndex]
    assert(focusedCol?.colId != null)
    tableNewArgs.pasteFromClipboard(data, {
      rowIndex: focused.rowIndex,
      colId: focusedCol.colId as Ast.AstId,
    })
    if (code !== expected) {
      expect(onUpdate).toHaveBeenCalledOnce()
    }
    if (importExpected) expect(addMissingImports).toHaveBeenCalled()
    else expect(addMissingImports).not.toHaveBeenCalled()
  },
)
