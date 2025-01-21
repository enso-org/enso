import { NEW_COLUMN_ID } from '@/components/GraphEditor/widgets/WidgetTableEditor/tableInputArgument'
import { WidgetInput } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { ToValue } from '@/util/reactivity'
import {
  CellEditingStartedEvent,
  CellEditingStoppedEvent,
  CellPosition,
  StartEditingCellParams,
} from 'ag-grid-enterprise'
import { computed, ref, toValue, watch } from 'vue'

interface EditedCell {
  rowIndex: number | 'header'
  colKey: string
}

export function useTableEditHandlers(
  gridApi: ToValue<
    | {
        stopEditing(cancel: boolean): void
        startEditingCell(editedCell: StartEditingCellParams): void
        getEditingCells(): Array<CellPosition>
      }
    | undefined
  >,
  input: ToValue<WidgetInput>,
  colDefs: ToValue<{ colId: string }[]>,
  pointerdown: (handler: WidgetEditHandler, event: PointerEvent) => void | boolean,
) {
  const columnIndexById = computed(
    () => new Map(toValue(colDefs).map((col, index) => [col.colId, index])),
  )
  const firstColumn = computed(() => toValue(colDefs)[1]) // The 0 col is un-editable row index.

  const editedCell = ref<EditedCell>()
  let revertChangesCb: (() => void) | undefined
  function syncGridWithEditedCell(cell = editedCell.value) {
    const api = toValue(gridApi)
    if (!api) return
    const editedInGrid = api.getEditingCells()[0]
    if (cell == null || cell.rowIndex === 'header') {
      api.stopEditing(false)
    } else if (
      editedInGrid?.rowIndex !== cell.rowIndex ||
      editedInGrid?.column.getColId() !== cell.colKey
    ) {
      api.startEditingCell({ rowIndex: cell.rowIndex, colKey: cell.colKey })
    }
  }
  watch(editedCell, (cell) => {
    syncGridWithEditedCell(cell)
    if (cell != null && !handler.isActive()) {
      handler.start()
    } else if (cell == null && handler.isActive()) {
      handler.end()
    }
  })

  const handler = WidgetEditHandler.New('WidgetTableEditor', toValue(input), {
    cancel() {
      revertChangesCb?.()
      editedCell.value = undefined
    },
    end() {
      editedCell.value = undefined
    },
    pointerdown: (event) => pointerdown(handler, event),
    suspend: () => {
      return {
        resume: () => syncGridWithEditedCell(),
      }
    },
  })

  const gridEventHandlers = {
    cellEditingStarted(event: CellEditingStartedEvent) {
      console.log('EVENT', event)
      revertChangesCb = () => toValue(gridApi)?.stopEditing(true)
      editedCell.value =
        event.rowIndex != null ?
          { rowIndex: event.rowIndex, colKey: event.column.getColId() }
        : undefined
      if (!handler.isActive()) {
        handler.start()
      }
    },
    cellEditingStopped(_event: CellEditingStoppedEvent) {
      const api = toValue(gridApi)
      if (!api?.getEditingCells().length && handler.isActive()) {
        handler.end()
      }
    },
    rowDataUpdated() {
      syncGridWithEditedCell()
    },
    keydown(event: KeyboardEvent) {
      console.log('EVENT', event)
      const handler =
        event.code === 'Tab' ? tabPressed
        : event.code === 'Enter' ? enterPressed
        : undefined
      if (handler?.() === false) {
        event.stopPropagation()
      }
    },
  }

  const headerEventHandlers = {
    headerEditingStarted(colKey: string, revertChanges: () => void) {
      if (editedCell.value?.rowIndex != 'header' || editedCell.value?.colKey !== colKey) {
        editedCell.value = { rowIndex: 'header', colKey }
        if (!handler.isActive()) {
          handler.start()
        }
      }
      revertChangesCb = revertChanges
    },

    headerEditingStopped(colId: string) {
      if (editedCell.value?.rowIndex === 'header' && editedCell.value.colKey === colId) {
        editedCell.value = undefined
      }
    },
  }

  function tabPressed() {
    // When cell is edited, AgGrid handles tab correctly.
    if (editedCell.value == null || editedCell.value.rowIndex !== 'header') return false
    const currentIndex = columnIndexById.value.get(editedCell.value.colKey)
    if (currentIndex == null) return
    const columnDefs = toValue(colDefs)
    const colOnRight = columnDefs[currentIndex + 1]
    console.log(colOnRight)
    if (colOnRight != null && colOnRight.colId != NEW_COLUMN_ID) {
      editedCell.value = { rowIndex: 'header', colKey: colOnRight.colId }
    } else if (firstColumn.value != null) {
      editedCell.value = { rowIndex: 0, colKey: firstColumn.value.colId }
    }
  }

  function enterPressed() {
    if (editedCell.value == null || firstColumn.value == null) return false

    const nextRow = editedCell.value.rowIndex === 'header' ? 0 : editedCell.value.rowIndex + 1
    editedCell.value = { rowIndex: nextRow, colKey: firstColumn.value.colId }
  }

  return { handler, editedCell, gridEventHandlers, headerEventHandlers }
}
