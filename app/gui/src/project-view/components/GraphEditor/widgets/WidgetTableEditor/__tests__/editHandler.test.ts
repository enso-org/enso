import { PortId } from '@/providers/portInfo'
import { WidgetInput } from '@/providers/widgetRegistry'
import { test, vi } from 'vitest'
import { EditedCell } from '../editHandlers'
import { NEW_COLUMN_ID, ROW_INDEX_COLUMN_ID } from '../tableInputArgument'

const colDefs = [
  { colId: ROW_INDEX_COLUMN_ID },
  { colId: 'col1' },
  { colId: 'col2' },
  { colId: NEW_COLUMN_ID },
]
const input = { portId: 'input' as PortId } as WidgetInput

test.each([[{ colKey: 'col1', rowIndex: 0 }]])('Start, switch and stop editing', (selections) => {
  let editedCell: EditedCell | undefined
  let cancelled = false
  const gridApi = {
    stopEditing: vi.fn((can) => {
      editedCell = undefined
      cancelled = can
    }),
    startEditingCell: vi.fn((cell) => (editedCell = cell)),
    getEditingCells: vi.fn(
      () =>
        editedCell && [
          { rowIndex: editedCell.rowIndex, column: { getColId: () => editedCell!.colKey } },
        ],
    ),
  }
})
