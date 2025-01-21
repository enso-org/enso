import {
  ColumnDef,
  NEW_COLUMN_ID,
  ROW_INDEX_HEADER,
  RowData,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableInputArgument'
import { WidgetInput } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { ToValue } from '@/util/reactivity'
import {
  CellEditingStartedEvent,
  CellEditingStoppedEvent,
  Column,
  StartEditingCellParams,
} from 'ag-grid-enterprise'
import { computed, ref, toValue } from 'vue'

interface EditedCell {
  rowIndex: number
  colKey: Column<RowData>
}

export function useTableEditHandlers(
  gridApi: ToValue<
    | {
        stopEditing(cancel: boolean): void
        startEditingCell(editedCell: StartEditingCellParams): void
        getEditingCells(): Array<unknown>
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

  class CellEditing {
    handler: WidgetEditHandler
    editedCell: EditedCell | undefined
    supressNextStopEditEvent: boolean = false

    constructor() {
      this.handler = WidgetEditHandler.New('WidgetTableEditor.cellEditHandler', toValue(input), {
        cancel() {
          toValue(gridApi)?.stopEditing(true)
        },
        end() {
          toValue(gridApi)?.stopEditing(false)
        },
        pointerdown: (event) => pointerdown(this.handler, event),
        suspend: () => {
          return {
            resume: () => this.editedCell && toValue(gridApi)?.startEditingCell(this.editedCell),
          }
        },
      })
    }

    cellEditedInGrid(event: CellEditingStartedEvent) {
      this.editedCell =
        event.rowIndex != null ? { rowIndex: event.rowIndex, colKey: event.column } : undefined
      if (!this.handler.isActive()) {
        this.handler.start()
      }
    }

    cellEditingStoppedInGrid(_event: CellEditingStoppedEvent) {
      const api = toValue(gridApi)
      if (this.supressNextStopEditEvent && this.editedCell) {
        this.supressNextStopEditEvent = false
        // If row data changed, the editing will be stopped, but we want to continue it.
        api?.startEditingCell(this.editedCell)
      } else if (!api?.getEditingCells().length && this.handler.isActive()) {
        this.handler.end()
      }
    }

    rowDataChanged() {
      if (this.handler.isActive()) {
        this.supressNextStopEditEvent = true
      }
    }

    enterPressed() {
      if (this.editedCell != null) {
        const api = toValue(gridApi)
        if (firstColumn.value != null) {
          api?.startEditingCell({
            rowIndex: this.editedCell.rowIndex + 1,
            colKey: firstColumn.value.colId,
          })
        } else {
          api?.stopEditing(false)
        }
        return true
      } else {
        return false
      }
    }
  }

  class HeaderEditing {
    handler: WidgetEditHandler
    editedColId = ref<string>()
    revertChangesCallback: (() => void) | undefined

    constructor() {
      this.handler = WidgetEditHandler.New('WidgetTableEditor.headerEditHandler', toValue(input), {
        cancel: () => {
          this.revertChangesCallback?.()
          this.editedColId.value = undefined
        },
        end: () => {
          this.editedColId.value = undefined
        },
        pointerdown: (event) => pointerdown(this.handler, event),
      })
    }

    headerEditedInGrid(colId: string, revertChanges: () => void) {
      if (this.editedColId.value !== colId) {
        this.editedColId.value = colId
        if (!this.handler.isActive()) {
          this.handler.start()
        }
      }
      this.revertChangesCallback = revertChanges
    }

    headerEditingStoppedInGrid(colId: string) {
      if (this.editedColId.value === colId) {
        this.revertChangesCallback = undefined
        this.editedColId.value = undefined
        if (this.handler.isActive()) {
          this.handler.end()
        }
      }
    }

    tabPressed() {
      if (!this.editedColId.value) return
      const currentIndex = columnIndexById.value.get(this.editedColId.value)
      if (currentIndex == null) return
      const columnDefs = toValue(colDefs)
      const colOnRight = columnDefs[currentIndex + 1]
      if (colOnRight != null && colOnRight.colId != NEW_COLUMN_ID) {
        this.editedColId.value = colOnRight.colId
      } else if (firstColumn.value != null) {
        toValue(gridApi)?.startEditingCell({ rowIndex: 0, colKey: firstColumn.value.colId })
      }
    }

    enterPressed() {
      if (this.editedColId.value != null) {
        this.handler.end()
        if (firstColumn.value != null) {
          toValue(gridApi)?.startEditingCell({ rowIndex: 0, colKey: firstColumn.value.colId })
        }
        return true
      }
      return false
    }
  }

  return { CellEditing, HeaderEditing }
}
