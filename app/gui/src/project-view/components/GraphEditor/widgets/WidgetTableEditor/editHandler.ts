import { NEW_COLUMN_ID } from '@/components/GraphEditor/widgets/WidgetTableEditor/tableInputArgument'
import { WidgetEditHandler, WidgetEditHooks } from '@/providers/widgetRegistry/editHandler'
import { ToValue } from '@/util/reactivity'
import { CellPosition, StartEditingCellParams } from 'ag-grid-enterprise'
import { computed, ref, toValue, watch } from 'vue'

export interface EditedCell {
  rowIndex: number | 'header'
  colKey: string
}

/**
 * A composable containing logic for WidgetTableEditor's edit handler.
 *
 * Contains logic of synchronizing state between AgGrid cells and our custom headers,
 * and react for user input received from them.
 */
export function useTableEditHandler(
  gridApi: ToValue<
    | {
        stopEditing(cancel: boolean): void
        startEditingCell(editedCell: StartEditingCellParams): void
        getEditingCells(): Array<CellPosition>
      }
    | undefined
  >,
  colDefs: ToValue<{ colId: string }[]>,
  widgetHandlerConstructor: (hooks: WidgetEditHooks) => WidgetEditHandler,
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

  const handler = widgetHandlerConstructor({
    cancel() {
      revertChangesCb?.()
      editedCell.value = undefined
    },
    end() {
      editedCell.value = undefined
    },
  })

  const gridEventHandlers = {
    cellEditingStarted(event: { rowIndex: number | undefined; column: { getColId(): string } }) {
      revertChangesCb = () => toValue(gridApi)?.stopEditing(true)
      editedCell.value =
        event.rowIndex != null ?
          { rowIndex: event.rowIndex, colKey: event.column.getColId() }
        : undefined
      if (!handler.isActive()) {
        handler.start()
      }
    },
    cellEditingStopped(event: { rowIndex: number | undefined; column: { getColId(): string } }) {
      const api = toValue(gridApi)
      if (
        event.rowIndex === editedCell.value?.rowIndex &&
        event.column.getColId() === editedCell.value?.colKey &&
        !api?.getEditingCells().length &&
        handler.isActive()
      ) {
        handler.end()
      }
    },
    rowDataUpdated() {
      // Sometimes edited cell appears only after updating row data, for example after filling
      // value in "new row" and pressing enter.
      syncGridWithEditedCell()
    },
    keydown(event: KeyboardEvent) {
      const handler =
        event.code === 'Tab' ? tabPressed
        : event.code === 'Enter' ? enterPressed
        : undefined
      if (handler && handler() !== false) {
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
        if (!toValue(gridApi)?.getEditingCells().length && handler.isActive()) {
          handler.end()
        }
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

  return {
    /** WidgetEdit handler representing grid editing */
    handler,
    editedCell,
    /** All event handlers for AgGridVue component */
    gridEventHandlers,
    /**
     * Handlers for header events - they should be called in all `onHeader...` methods in header
     * params
     */
    headerEventHandlers,
  }
}
