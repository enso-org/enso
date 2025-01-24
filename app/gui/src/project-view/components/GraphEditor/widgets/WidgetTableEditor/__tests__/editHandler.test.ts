import { InteractionHandler, provideInteractionHandler } from '@/providers/interactionHandler'
import { PortId } from '@/providers/portInfo'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { provideWidgetTree, useCurrentEdit } from '@/providers/widgetTree'
import { withSetup } from '@/util/testing'
import { CellPosition } from 'ag-grid-enterprise'
import { expect, test, vi } from 'vitest'
import { nextTick } from 'vue'
import { EditedCell, useTableEditHandler } from '../editHandlers'
import { NEW_COLUMN_ID, ROW_INDEX_COLUMN_ID } from '../tableInputArgument'

const colDefs = [
  { colId: ROW_INDEX_COLUMN_ID },
  { colId: 'col1' },
  { colId: 'col2' },
  { colId: NEW_COLUMN_ID },
]

function fixture() {
  const interactionHandler = new InteractionHandler()
  /** A "simulated" state of AgGrid. */
  const gridState: {
    editedCell: (EditedCell & { rowIndex: number }) | undefined
    editCanceled: boolean
  } = {
    editedCell: undefined,
    editCanceled: false,
  }
  const gridApi = {
    stopEditing: vi.fn((cancelled) => {
      gridState.editedCell = undefined
      gridState.editCanceled = cancelled
    }),
    startEditingCell: vi.fn((cell) => {
      gridState.editedCell = cell
    }),
    getEditingCells: vi.fn(() =>
      gridState.editedCell ?
        [
          {
            rowIndex: gridState.editedCell.rowIndex,
            column: { getColId: () => gridState.editedCell!.colKey },
          } as CellPosition,
        ]
      : [],
    ),
  }

  const composable = useTableEditHandler(
    gridApi,
    colDefs,
    (hooks) =>
      new WidgetEditHandler(
        'port' as PortId,
        hooks,
        undefined,
        useCurrentEdit(),
        interactionHandler,
      ),
  )

  /** Simulate user action of starting/stopping cell/header editing */
  const editedInGrid = async (cell: EditedCell | undefined) => {
    const oldEdit = gridState.editedCell
    // Update internal state first.
    if (cell != null && cell?.rowIndex !== 'header') {
      gridState.editedCell = { colKey: cell.colKey, rowIndex: cell.rowIndex }
    } else {
      gridState.editedCell = undefined
    }
    // Inform that previous edit stopped, as this is what AgGrid does.
    if (oldEdit != null) {
      composable.gridEventHandlers.cellEditingStopped()
    }
    // AgGrid does not keep separate state of edited headers: it's the same as composable's ref.
    if (cell == null && composable.editedCell.value?.rowIndex === 'header') {
      composable.headerEventHandlers.headerEditingStopped(composable.editedCell.value.colKey)
    }
    if (cell != null && cell.rowIndex !== 'header') {
      composable.gridEventHandlers.cellEditingStarted({
        rowIndex: cell.rowIndex,
        column: { getColId: () => cell.colKey },
      })
    }
    if (cell != null && cell.rowIndex === 'header') {
      composable.headerEventHandlers.headerEditingStarted(cell.colKey, () => {})
    }
    // wait for all watches being updated
    await nextTick()
  }

  return { gridState, composable, editedInGrid }
}

test.each([
  [[{ colKey: 'col1', rowIndex: 0 }]],
  [
    [
      { colKey: 'col1', rowIndex: 0 },
      { colKey: 'col2', rowIndex: 1 },
    ],
  ],
  [
    [
      { colKey: 'col1', rowIndex: 0 },
      { colKey: 'col2', rowIndex: 'header' as const },
    ],
  ],
  [
    [
      { colKey: 'col1', rowIndex: 'header' as const },
      { colKey: 'col2', rowIndex: 1 },
    ],
  ],
  [
    [
      { colKey: 'col1', rowIndex: 'header' as const },
      { colKey: 'col2', rowIndex: 'header' as const },
    ],
  ],
])('User edit sequence: select %s and then stop editing', async (selections) => {
  const {
    gridState,
    composable: { handler, editedCell, gridEventHandlers, headerEventHandlers },
    editedInGrid,
  } = fixture()

  for (const selection of selections) {
    await editedInGrid(selection)

    expect(handler.isActive()).toBeTruthy()
    expect(editedCell.value).toEqual(selection)
    if (selection.rowIndex !== 'header') {
      expect(gridState.editedCell).toEqual(selection)
    } else {
      expect(gridState.editedCell).toBeUndefined()
    }
    expect(gridState.editCanceled).toBeFalsy()
  }
  await editedInGrid(undefined)
  expect(handler.isActive()).toBeFalsy()
  expect(editedCell.value).toBeUndefined()
})

test.each([
  { initial: { colKey: 'col1', rowIndex: 0 }, expected: { colKey: 'col1', rowIndex: 1 } },
])('Pressing `enter` while editing $initial', async ({ initial, expected }) => {
  const {
    gridState,
    composable: { handler, editedCell },
    edited,
  }
})
