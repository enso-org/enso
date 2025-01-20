import {
  CELLS_LIMIT,
  DEFAULT_COLUMN_PREFIX,
  NEW_COLUMN_ID,
  ROW_INDEX_HEADER,
  RowData,
  tableInputCallMayBeHandled,
  useTableInputArgument,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableInputArgument'
import { MenuItem } from '@/components/shared/AgGridTableView.vue'
import { WidgetInput } from '@/providers/widgetRegistry'
import { SuggestionDb } from '@/stores/suggestionDatabase'
import { makeType } from '@/stores/suggestionDatabase/mockSuggestion'
import { assert } from '@/util/assert'
import { Ast } from '@/util/ast'
import { GetContextMenuItems, GetMainMenuItems } from 'ag-grid-enterprise'
import { expect, test, vi } from 'vitest'
import { assertDefined } from 'ydoc-shared/util/assert'

function suggestionDbWithNothing() {
  const db = new SuggestionDb()
  db.set(1, makeType('Standard.Base.Nothing.Nothing'))
  return db
}

function generateTableOfOnes(rows: number, cols: number) {
  const code = `Table.input [${[...Array(cols).keys()].map((i) => `['Column #${i}', [${Array(rows).fill("'1'").join(',')}]]`).join(',')}]`
  const ast = Ast.parseExpression(code)
  assertDefined(ast)
  return ast
}

const expectedRowIndexColumnDef = { headerName: ROW_INDEX_HEADER, cellClass: 'rowIndexCell' }
const expectedNewColumnDef = { cellClass: 'newColumnCell' }

const CELLS_LIMIT_SQRT = Math.sqrt(CELLS_LIMIT)
assert(CELLS_LIMIT_SQRT === Math.floor(CELLS_LIMIT_SQRT))

test.each([
  {
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', 'six']], ['empty', [Nothing, Standard.Base.Nothing, Nothing]]]",
    expectedColumnDefs: [
      expectedRowIndexColumnDef,
      { headerName: 'a' },
      { headerName: 'b' },
      { headerName: 'empty' },
      expectedNewColumnDef,
    ],
    expectedRows: [
      { [ROW_INDEX_HEADER]: 0, a: '1', b: '4', empty: null, '': null },
      { [ROW_INDEX_HEADER]: 1, a: '2', b: '5', empty: null, '': null },
      { [ROW_INDEX_HEADER]: 2, a: '3', b: 'six', empty: null, '': null },
      { [ROW_INDEX_HEADER]: 3, a: null, b: null, empty: null, '': null },
    ],
  },
  {
    code: 'Table.input []',
    expectedColumnDefs: [expectedRowIndexColumnDef, expectedNewColumnDef],
    expectedRows: [{ [ROW_INDEX_HEADER]: 0, '': null }],
  },
  {
    code: 'Table.input',
    expectedColumnDefs: [expectedRowIndexColumnDef, expectedNewColumnDef],
    expectedRows: [{ [ROW_INDEX_HEADER]: 0, '': null }],
  },
  {
    code: 'Table.input _',
    expectedColumnDefs: [expectedRowIndexColumnDef, expectedNewColumnDef],
    expectedRows: [{ [ROW_INDEX_HEADER]: 0, '': null }],
  },
  {
    code: 'Table.input [["a", []]]',
    expectedColumnDefs: [expectedRowIndexColumnDef, { headerName: 'a' }, expectedNewColumnDef],
    expectedRows: [{ [ROW_INDEX_HEADER]: 0, a: null, '': null }],
  },
  {
    code: "Table.input [['a', ['1',,'2']], ['b', ['3', '4',]], ['c', [, '5', '6']], ['d', [,,]]]",
    expectedColumnDefs: [
      expectedRowIndexColumnDef,
      { headerName: 'a' },
      { headerName: 'b' },
      { headerName: 'c' },
      { headerName: 'd' },
      expectedNewColumnDef,
    ],
    expectedRows: [
      { [ROW_INDEX_HEADER]: 0, a: '1', b: '3', c: null, d: null, '': null },
      { [ROW_INDEX_HEADER]: 1, a: null, b: '4', c: '5', d: null, '': null },
      { [ROW_INDEX_HEADER]: 2, a: '2', b: null, c: '6', d: null, '': null },
      { [ROW_INDEX_HEADER]: 3, a: null, b: null, c: null, d: null, '': null },
    ],
  },
])('Read table from $code', ({ code, expectedColumnDefs, expectedRows }) => {
  const ast = Ast.parseExpression(code)
  assertDefined(ast)
  expect(tableInputCallMayBeHandled(ast)).toBeTruthy()
  const input = WidgetInput.FromAst(ast)
  const startEdit = vi.fn()
  const addMissingImports = vi.fn()
  const onUpdate = vi.fn()
  const tableNewArgs = useTableInputArgument(
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
  {
    rows: Math.floor(CELLS_LIMIT / 2) + 1,
    cols: 1,
    expectNewRowEnabled: true,
    expectNewColEnabled: false,
  },
  {
    rows: 1,
    cols: Math.floor(CELLS_LIMIT / 2) + 1,
    expectNewRowEnabled: false,
    expectNewColEnabled: true,
  },
  {
    rows: 1,
    cols: CELLS_LIMIT,
    expectNewRowEnabled: false,
    expectNewColEnabled: false,
  },
  {
    rows: CELLS_LIMIT,
    cols: 1,
    expectNewRowEnabled: false,
    expectNewColEnabled: false,
  },
  {
    rows: CELLS_LIMIT_SQRT,
    cols: CELLS_LIMIT_SQRT,
    expectNewRowEnabled: false,
    expectNewColEnabled: false,
  },
])(
  'Allowed actions in table near limit (rows: $rows, cols: $cols)',
  ({ rows, cols, expectNewRowEnabled, expectNewColEnabled }) => {
    const input = WidgetInput.FromAst(generateTableOfOnes(rows, cols))
    const tableNewArgs = useTableInputArgument(
      input,
      { startEdit: vi.fn(), addMissingImports: vi.fn() },
      suggestionDbWithNothing(),
      vi.fn(),
    )
    expect(tableNewArgs.rowData.value.length).toBe(rows + (expectNewRowEnabled ? 1 : 0))
    const lastColDef = tableNewArgs.columnDefs.value[tableNewArgs.columnDefs.value.length - 1]
    assert(lastColDef?.headerComponentParams.columnParams.type === 'newColumn')
    expect(lastColDef.headerComponentParams.columnParams.enabled ?? true).toBe(expectNewColEnabled)
  },
)

test.each([
  'Table.input 14',
  'Table.input array1',
  "Table.input ['a', ['123']]",
  "Table.input [['a', [123]]]",
  "Table.input [['a', ['123']], ['b', ['124'], []]]",
  "Table.input [['a', ['123']], ['a'.repeat 170, ['123']]]",
  "Table.input [['a', ['1', '2', '3', 3 + 1]]]",
])('"%s" is not valid input for Table Editor Widget', (code) => {
  const ast = Ast.parseExpression(code)
  assertDefined(ast)
  expect(tableInputCallMayBeHandled(ast)).toBeFalsy()
})

function tableEditFixture(code: string, expectedCode: string) {
  const ast = Ast.parseBlock(code)
  const firstStatement = [...ast.statements()][0]
  assert(firstStatement instanceof Ast.MutableExpressionStatement)
  const inputAst = firstStatement.expression
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
  const tableNewArgs = useTableInputArgument(
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
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    description: 'Edit value',
    edit: { column: 1, row: 1, value: 'two' },
    expected: "Table.input [['a', ['1', 'two', '3']], ['b', ['4', '5', '6']]]",
  },
  {
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    description: 'Put blank value',
    edit: { column: 2, row: 1, value: '' },
    expected: "Table.input [['a', ['1', '2', '3']], ['b', ['4', Nothing, '6']]]",
    importExpected: true,
  },

  {
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    description: 'Add new row',
    edit: { column: 1, row: 3, value: '4.5' },
    expected: "Table.input [['a', ['1', '2', '3', '4.5']], ['b', ['4', '5', '6', Nothing]]]",
    importExpected: true,
  },
  {
    code: "Table.input [['a', []], ['b', []]]",
    description: 'Add first row',
    edit: { column: 2, row: 0, value: 'val' },
    expected: "Table.input [['a', [Nothing]], ['b', ['val']]]",
    importExpected: true,
  },
  {
    code: "Table.input [['a', ['1', ,'3']]]",
    description: 'Set missing value',
    edit: { column: 1, row: 1, value: '2' },
    expected: "Table.input [['a', ['1', '2' ,'3']]]",
  },
  {
    code: "Table.input [['a', [, '2', '3']]]",
    description: 'Set missing value at first row',
    edit: { column: 1, row: 0, value: '1' },
    expected: "Table.input [['a', ['1', '2', '3']]]",
  },
  {
    code: "Table.input [['a', ['1', '2',]]]",
    description: 'Set missing value at last row',
    edit: { column: 1, row: 2, value: '3' },
    expected: "Table.input [['a', ['1', '2', '3']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']], ['a', ['3', '4']]]",
    description: 'Edit with duplicated column name',
    edit: { column: 1, row: 1, value: '5' },
    expected: "Table.input [['a', ['1', '5']], ['a', ['3', '4']]]",
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
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    expected: `Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']], ['${DEFAULT_COLUMN_PREFIX}3', [Nothing, Nothing, Nothing]]]`,
    importExpected: true,
  },
  {
    code: 'Table.input []',
    expected: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', []]]`,
  },
  {
    code: 'Table.input',
    expected: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', []]]`,
  },
  {
    code: 'Table.input _',
    expected: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', []]]`,
  },
])('Add column to table $code', ({ code, expected, importExpected }) => {
  const { tableNewArgs, onUpdate, addMissingImports } = tableEditFixture(code, expected)
  const newColumnDef = tableNewArgs.columnDefs.value.find(
    (colDef) => colDef.colId === NEW_COLUMN_ID,
  )
  assert(newColumnDef != null)
  assert(newColumnDef.headerComponentParams?.columnParams.type === 'newColumn')
  assert(newColumnDef.headerComponentParams.columnParams.newColumnRequested != null)
  newColumnDef.headerComponentParams.columnParams.newColumnRequested()
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
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    removedRowIndex: 0,
    expected: "Table.input [['a', ['2', '3']], ['b', ['5', '6']]]",
  },
  {
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    removedRowIndex: 1,
    expected: "Table.input [['a', ['1', '3']], ['b', ['4', '6']]]",
  },
  {
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    removedRowIndex: 2,
    expected: "Table.input [['a', ['1', '2']], ['b', ['4', '5']]]",
  },
  {
    code: "Table.input [['a', ['1']], ['b', ['4']]]",
    removedRowIndex: 0,
    expected: "Table.input [['a', []], ['b', []]]",
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
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']], ['c', ['5', '6']]]",
    removedColIndex: 1,
    expected: "Table.input [['b', ['3', '4']], ['c', ['5', '6']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']], ['c', ['5', '6']]]",
    removedColIndex: 2,
    expected: "Table.input [['a', ['1', '2']], ['c', ['5', '6']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']], ['c', ['5', '6']]]",
    removedColIndex: 3,
    expected: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']]]",
    removedColIndex: 1,
    expected: 'Table.input []',
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
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']], ['c', ['5', '6']]]",
    fromIndex: 1,
    toIndex: 3,
    expected: "Table.input [['b', ['3', '4']], ['c', ['5', '6']], ['a', ['1', '2']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']], ['c', ['5', '6']]]",
    fromIndex: 3,
    toIndex: 2,
    expected: "Table.input [['a', ['1', '2']], ['c', ['5', '6']], ['b', ['3', '4']]]",
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
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    fromIndex: 1,
    toIndex: 2,
    expected: "Table.input [['a', ['1', '3', '2']], ['b', ['4', '6', '5']]]",
  },
  {
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    fromIndex: 2,
    toIndex: 0,
    expected: "Table.input [['a', ['3', '1', '2']], ['b', ['6', '4', '5']]]",
  },
  {
    code: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
    fromIndex: 1,
    toIndex: -1,
    expected: "Table.input [['a', ['1', '2', '3']], ['b', ['4', '5', '6']]]",
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
    code: 'Table.input',
    focused: { rowIndex: 0, colIndex: 1 },
    data: [
      ['1', '3'],
      ['2', '4'],
    ],
    expected: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', ['1', '2']], ['${DEFAULT_COLUMN_PREFIX}2', ['3', '4']]]`,
  },
  {
    code: 'Table.input []',
    focused: { rowIndex: 0, colIndex: 1 },
    data: [
      ['1', '3'],
      ['2', '4'],
    ],
    expected: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', ['1', '2']], ['${DEFAULT_COLUMN_PREFIX}2', ['3', '4']]]`,
  },
  {
    code: 'Table.input []',
    focused: { rowIndex: 0, colIndex: 1 },
    data: [['a single cell']],
    expected: `Table.input [['${DEFAULT_COLUMN_PREFIX}1', ['a single cell']]]`,
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 0, colIndex: 1 },
    data: [['a single cell']],
    expected: "Table.input [['a', ['a single cell', '2']], ['b', ['3', '4']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 1, colIndex: 2 },
    data: [['a single cell']],
    expected: "Table.input [['a', ['1', '2']], ['b', ['3', 'a single cell']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 2, colIndex: 2 },
    data: [['a single cell']],
    expected: "Table.input [['a', ['1', '2', Nothing]], ['b', ['3', '4', 'a single cell']]]",
    importExpected: true,
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 1, colIndex: 3 },
    data: [['a single cell']],
    expected: `Table.input [['a', ['1', '2']], ['b', ['3', '4']], ['${DEFAULT_COLUMN_PREFIX}3', [Nothing, 'a single cell']]]`,
    importExpected: true,
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 0, colIndex: 1 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: "Table.input [['a', ['5', '6']], ['b', ['7', '8']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 1, colIndex: 1 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: "Table.input [['a', ['1', '5', '6']], ['b', ['3', '7', '8']]]",
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 0, colIndex: 2 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: `Table.input [['a', ['1', '2']], ['b', ['5', '6']], ['${DEFAULT_COLUMN_PREFIX}3', ['7', '8']]]`,
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 1, colIndex: 2 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: `Table.input [['a', ['1', '2', Nothing]], ['b', ['3', '5', '6']], ['${DEFAULT_COLUMN_PREFIX}3', [Nothing, '7', '8']]]`,
    importExpected: true,
  },
  {
    code: "Table.input [['a', ['1', '2']], ['b', ['3', '4']]]",
    focused: { rowIndex: 2, colIndex: 2 },
    data: [
      ['5', '7'],
      ['6', '8'],
    ],
    expected: `Table.input [['a', ['1', '2', Nothing, Nothing]], ['b', ['3', '4', '5', '6']], ['${DEFAULT_COLUMN_PREFIX}3', [Nothing, Nothing, '7', '8']]]`,
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

test('Pasted data which would exceed cells limit is truncated', () => {
  const initialRows = CELLS_LIMIT_SQRT - 2
  const initialCols = CELLS_LIMIT_SQRT - 1
  const ast = generateTableOfOnes(initialRows, initialCols)
  const input = WidgetInput.FromAst(ast)
  const startEdit = vi.fn(() => ast.module.edit())
  const onUpdate = vi.fn((update) => {
    const inputAst = update.edit!.getVersion(ast)
    // We expect the table to be fully extended, so the number of cells (numbers or Nothings) should be equal to the limit.
    let cellCount = 0
    inputAst.visitRecursive((ast: Ast.Ast | Ast.Token) => {
      if (ast instanceof Ast.Token) return
      if (ast instanceof Ast.TextLiteral && ast.code().startsWith("'Column #")) return
      if (ast instanceof Ast.TextLiteral || ast.code() === 'Nothing') cellCount++
    })
    expect(cellCount).toBe(CELLS_LIMIT)
  })
  const addMissingImports = vi.fn()
  const tableNewArgs = useTableInputArgument(
    input,
    { startEdit, addMissingImports },
    suggestionDbWithNothing(),
    onUpdate,
  )
  const focusedCol = tableNewArgs.columnDefs.value[initialCols - 2]
  assert(focusedCol?.colId != null)
  tableNewArgs.pasteFromClipboard(Array(4).fill(Array(4).fill('2')), {
    rowIndex: initialRows - 2,
    colId: focusedCol.colId,
  })
  expect(onUpdate).toHaveBeenCalledOnce()
})
