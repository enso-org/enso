<script setup lang="ts">
import { WidgetInputIsSpecificMethodCall } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import TableHeader from '@/components/GraphEditor/widgets/WidgetTableEditor/TableHeader.vue'
import {
  CELLS_LIMIT,
  tableInputCallMayBeHandled,
  useTableInputArgument,
  type RowData,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableInputArgument'
import ResizeHandles from '@/components/ResizeHandles.vue'
import AgGridTableView from '@/components/shared/AgGridTableView.vue'
import { injectGraphNavigator } from '@/providers/graphNavigator'
import { useTooltipRegistry } from '@/providers/tooltipRegistry'
import { Score, defineWidget, widgetProps } from '@/providers/widgetRegistry'
import { WidgetEditHandler } from '@/providers/widgetRegistry/editHandler'
import { useGraphStore } from '@/stores/graph'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { Rect } from '@/util/data/rect'
import { Vec2 } from '@/util/data/vec2'
import { useToast } from '@/util/toast'
import '@ag-grid-community/styles/ag-grid.css'
import '@ag-grid-community/styles/ag-theme-alpine.css'
import type {
  CellEditingStartedEvent,
  CellEditingStoppedEvent,
  ColDef,
  Column,
  ColumnMovedEvent,
  ProcessDataFromClipboardParams,
  RowDragEndEvent,
} from 'ag-grid-enterprise'
import { computed, markRaw, ref } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'
import { z } from 'zod'

const props = defineProps(widgetProps(widgetDefinition))
const graph = useGraphStore()
const suggestionDb = useSuggestionDbStore()
const grid = ref<ComponentExposed<typeof AgGridTableView<RowData, any>>>()
const pasteWarning = useToast.warning()

const configSchema = z.object({ size: z.object({ x: z.number(), y: z.number() }) })
type Config = z.infer<typeof configSchema>

const DEFAULT_CFG: Config = { size: { x: 200, y: 150 } }

const config = computed(() => {
  const configObj = props.input.value.widgetMetadata('WidgetTableEditor')
  if (configObj == null) return DEFAULT_CFG
  const parsed = configSchema.safeParse(configObj)
  if (parsed.success) return parsed.data
  else {
    console.warn('Table Editor Widget: could not read config; invalid format: ', parsed.error)
    return DEFAULT_CFG
  }
})

const { rowData, columnDefs, moveColumn, moveRow, pasteFromClipboard } = useTableInputArgument(
  () => props.input,
  graph,
  suggestionDb.entries,
  props.onUpdate,
)

// === Edit Handlers ===

class CellEditing {
  handler: WidgetEditHandler
  editedCell: { rowIndex: number; colKey: Column<RowData> } | undefined
  supressNextStopEditEvent: boolean = false

  constructor() {
    this.handler = WidgetEditHandler.New('WidgetTableEditor.cellEditHandler', props.input, {
      cancel() {
        grid.value?.gridApi?.stopEditing(true)
      },
      end() {
        grid.value?.gridApi?.stopEditing(false)
      },
      suspend: () => {
        return {
          resume: () => this.editedCell && grid.value?.gridApi?.startEditingCell(this.editedCell),
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

  cellEditingStoppedInGrid(event: CellEditingStoppedEvent) {
    if (!this.handler.isActive()) return
    if (this.supressNextStopEditEvent && this.editedCell) {
      this.supressNextStopEditEvent = false
      // If row data changed, the editing will be stopped, but we want to continue it.
      grid.value?.gridApi?.startEditingCell(this.editedCell)
    } else {
      this.handler.end()
    }
  }

  rowDataChanged() {
    if (this.handler.isActive()) {
      this.supressNextStopEditEvent = true
    }
  }
}

const cellEditHandler = new CellEditing()

class HeaderEditing {
  handler: WidgetEditHandler
  stopEditingCallback: ((cancel: boolean) => void) | undefined

  constructor() {
    this.handler = WidgetEditHandler.New('WidgetTableEditor.headerEditHandler', props.input, {
      cancel: () => {
        this.stopEditingCallback?.(true)
      },
      end: () => {
        this.stopEditingCallback?.(false)
      },
    })
  }

  headerEditedInGrid(stopCb: (cancel: boolean) => void) {
    // If another header is edited, stop it (with the old callback).
    if (this.handler.isActive()) {
      this.stopEditingCallback?.(false)
    }
    this.stopEditingCallback = stopCb
    if (!this.handler.isActive()) {
      this.handler.start()
    }
  }

  headerEditingStoppedInGrid() {
    this.stopEditingCallback = undefined
    if (this.handler.isActive()) {
      this.handler.end()
    }
  }
}

const headerEditHandler = new HeaderEditing()

// === Resizing ===

const graphNav = injectGraphNavigator()

const size = computed(() => Vec2.FromXY(config.value.size))

const clientBounds = computed({
  get() {
    return new Rect(Vec2.Zero, size.value.scale(graphNav.scale))
  },
  set(value) {
    props.onUpdate({
      portUpdate: {
        origin: props.input.portId,
        metadataKey: 'WidgetTableEditor',
        metadata: { size: { x: value.width / graphNav.scale, y: value.height / graphNav.scale } },
      },
      directInteraction: false,
    })
  },
})

const widgetStyle = computed(() => {
  return {
    width: `${size.value.x}px`,
    height: `${size.value.y}px`,
  }
})

// === Column and Row Dragging ===

function onColumnMoved(event: ColumnMovedEvent<RowData>) {
  if (event.column && event.toIndex != null && event.finished) {
    moveColumn(event.column.getColId(), event.toIndex)
  }
}

function onRowDragEnd(event: RowDragEndEvent<RowData>) {
  if (event.node.data != null) {
    moveRow(event.node.data?.index, event.overIndex)
  }
}

// === Paste Handler ===

function processDataFromClipboard({ data, api }: ProcessDataFromClipboardParams<RowData>) {
  const focusedCell = api.getFocusedCell()
  if (focusedCell === null) console.warn('Pasting while no cell is focused!')
  else {
    const pasted = pasteFromClipboard(data, {
      rowIndex: focusedCell.rowIndex,
      colId: focusedCell.column.getColId(),
    })
    if (pasted.rows < data.length || pasted.columns < (data[0]?.length ?? 0)) {
      pasteWarning.show(`Truncated pasted data to keep table within ${CELLS_LIMIT} limit`)
    }
  }
  return []
}

// === Column Default Definition ===

const tooltipRegistry = useTooltipRegistry()
const defaultColDef: ColDef<RowData> = {
  editable: true,
  resizable: true,
  sortable: false,
  lockPinned: true,
  menuTabs: ['generalMenuTab'],
  headerComponentParams: {
    // TODO[ao]: we mark raw, because otherwise any change _inside_ tooltipRegistry causes the grid
    //  to be refreshed. Technically, shallowReactive should work here, but it does not,
    //  I don't know why
    tooltipRegistry: markRaw(tooltipRegistry),
    editHandlers: {
      onHeaderEditingStarted: headerEditHandler.headerEditedInGrid.bind(headerEditHandler),
      onHeaderEditingStopped: headerEditHandler.headerEditingStoppedInGrid.bind(headerEditHandler),
    },
  },
  cellStyle: { 'padding-left': 0, 'border-right': '1px solid #C0C0C0' },
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInputIsSpecificMethodCall({
    module: 'Standard.Table.Table',
    definedOnType: 'Standard.Table.Table.Table',
    name: 'input',
  }),
  {
    priority: 999,
    score: (props) => {
      if (!tableInputCallMayBeHandled(props.input.value)) return Score.Mismatch
      return Score.Perfect
    },
  },
  import.meta.hot,
)
</script>

<template>
  <div class="WidgetTableEditor" :style="widgetStyle">
    <Suspense>
      <AgGridTableView
        ref="grid"
        class="inner"
        :defaultColDef="defaultColDef"
        :columnDefs="columnDefs"
        :rowData="rowData"
        :getRowId="(row) => `${row.data.index}`"
        :components="{ agColumnHeader: TableHeader }"
        :stopEditingWhenCellsLoseFocus="true"
        :suppressDragLeaveHidesColumns="true"
        :suppressMoveWhenColumnDragging="true"
        :processDataFromClipboard="processDataFromClipboard"
        @keydown.enter.stop
        @keydown.arrow-left.stop
        @keydown.arrow-right.stop
        @keydown.arrow-up.stop
        @keydown.arrow-down.stop
        @keydown.backspace.stop
        @keydown.delete.stop
        @cellEditingStarted="cellEditHandler.cellEditedInGrid($event)"
        @cellEditingStopped="cellEditHandler.cellEditingStoppedInGrid($event)"
        @rowDataUpdated="cellEditHandler.rowDataChanged()"
        @pointerdown.stop
        @click.stop
        @columnMoved="onColumnMoved"
        @rowDragEnd="onRowDragEnd"
      />
    </Suspense>
    <ResizeHandles v-model="clientBounds" bottom right />
  </div>
</template>

<style scoped>
.WidgetTableEditor {
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: var(--node-port-border-radius);
  position: relative;
}

.inner {
  width: 100%;
  height: 100%;
}

:deep(.newColumnCell) {
  display: none;
}

:deep(.rowIndexCell) {
  color: rgba(0, 0, 0, 0.4);
}

/* Those two classes are copied from AgGridTableView component.
For some reason, Vue cannot load them there, probably because it is used also as Custom Element. */
:deep(.inner) {
  width: 100%;
  height: 100%;
}

/*
 * FIXME: This is a copy of the style defined within AgGridTableView, which has no effect here due to a bug.
 */
:deep(.ag-theme-alpine) {
  --ag-grid-size: 3px;
  --ag-list-item-height: 20px;
  --ag-background-color: var(--color-visualization-bg);
  --ag-odd-row-background-color: color-mix(in srgb, var(--color-visualization-bg) 98%, black);
  --ag-header-background-color: var(--color-visualization-bg);
  font-family: var(--font-mono);

  :deep(.ag-header) {
    background: linear-gradient(
      to top,
      var(--ag-odd-row-background-color),
      var(--ag-background-color)
    );
  }
}

/* Separate, actually widget-specific styling. */
.WidgetTableEditor:deep(.ag-root-wrapper) {
  --ag-wrapper-border-radius: var(--node-port-border-radius);
  border: none;
}
</style>
