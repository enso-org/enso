import {
  ColumnDef,
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
      console.log('CELL EDITED IN GRID', event)
      this.editedCell =
        event.rowIndex != null ? { rowIndex: event.rowIndex, colKey: event.column } : undefined
      if (!this.handler.isActive()) {
        this.handler.start()
      }
    }

    cellEditingStoppedInGrid(event: CellEditingStoppedEvent) {
      if (!this.handler.isActive()) return
      const api = toValue(gridApi)
      console.error('EDITING STOPPED', event, api?.getEditingCells())
      if (this.supressNextStopEditEvent && this.editedCell) {
        this.supressNextStopEditEvent = false
        // If row data changed, the editing will be stopped, but we want to continue it.
        api?.startEditingCell(this.editedCell)
      } else if (!api?.getEditingCells().length) {
        this.handler.end()
      }
    }

    rowDataChanged() {
      if (this.handler.isActive()) {
        this.supressNextStopEditEvent = true
      }
    }

    goToNextCell() {
      if (!this.editedCell) return
      const currentIndex = columnIndexById.value.get(this.editedCell.colKey.getId())
      if (currentIndex == null) return
      const columnDefs = toValue(colDefs)
      let nextCell
      const firstCol = columnDefs[1] // The 0 col is un-editable row index.
      const colOnRight = columnDefs[currentIndex + 1]
      if (colOnRight != null) {
        nextCell = { rowIndex: this.editedCell.rowIndex, colKey: colOnRight.colId }
      } else if (firstCol != null) {
        nextCell = { rowIndex: this.editedCell.rowIndex + 1, colKey: firstCol.colId }
      }
      if (nextCell) {
        toValue(gridApi)?.startEditingCell(nextCell)
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
  }

  return { CellEditing, HeaderEditing }
}
