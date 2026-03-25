<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { defineWidget, Score, widgetProps } from '$/providers/openedProjects/widgetRegistry'
import { WidgetEditHandler } from '$/providers/openedProjects/widgetRegistry/editHandler'
import { proxyRefs } from '$/utils/reactivity'
import AgGridTableView from '@/components/AgGridTableView.vue'
import ResizableWidget from '@/components/GraphEditor/ResizableWidget.vue'
import { WidgetInputIsSpecificMethodCall } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import TableHeader, {
  type HeaderParams,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/TableHeader.vue'
import { useTableEditHandler } from '@/components/GraphEditor/widgets/WidgetTableEditor/editHandler'
import {
  CELLS_LIMIT,
  type RowData,
  tableInputCallMayBeHandled,
  useTableInputArgument,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableInputArgument'
import { injectWidgetTree } from '@/providers/widgetTree'
import { targetIsOutside } from '@/util/autoBlur'
import { ProjectPath } from '@/util/projectPath'
import type { Identifier, QualifiedName } from '@/util/qualifiedName'
import { useToast } from '@/util/toast'
import '@ag-grid-community/styles/ag-grid.css'
import '@ag-grid-community/styles/ag-theme-alpine.css'
import type {
  ColDef,
  ColumnMovedEvent,
  ProcessDataFromClipboardParams,
  RowDragEndEvent,
} from 'ag-grid-enterprise'
import type { Result } from 'enso-common/src/utilities/data/result'
import { type ComponentInstance, computed, type ComputedRef, ref, watch } from 'vue'
import type { ComponentExposed } from 'vue-component-type-helpers'
import { z } from 'zod'

const props = defineProps(widgetProps(widgetDefinition))
const { suggestionDb, module } = useCurrentProject()
const tree = injectWidgetTree()
const grid = ref<
  ComponentInstance<typeof AgGridTableView<RowData, any>> &
    ComponentExposed<typeof AgGridTableView<RowData, any>>
>()
const pasteWarning = useToast.warning()

const configSchema = z.object({
  size: z.object({ x: z.number(), y: z.number() }),
})
type Config = z.infer<typeof configSchema>

const PREFERRED_HEIGHT_PX = 150
const DEFAULT_CFG: Config = { size: { x: 200, y: PREFERRED_HEIGHT_PX } }

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
  module,
  () => suggestionDb.value.entries,
  props.updateCallback,
)

const collapsedText = computed(() => {
  // One row is added only to allow adding new rows.
  const rows = rowData.value.length - 1
  return `Table (${rows} row${rows !== 1 ? 's' : ''})`
})

// Without this "cast" AgGridTableView gets confused when deducing its generic parameters.
const columnDefsTyped: ComputedRef<ColDef<RowData>[]> = columnDefs

// === Edit Handlers ===

const { editedCell, gridEventHandlers, headerEventHandlers } = useTableEditHandler(
  () => grid.value?.gridApi,
  columnDefs,
  (hooks) => {
    const handler = WidgetEditHandler.New(props, {
      ...hooks,
      pointerdown: (event) => {
        if (
          !(event.target instanceof HTMLInputElement) ||
          targetIsOutside(event, grid.value?.$el)
        ) {
          handler.value.end()
        } else {
          return false
        }
      },
    })
    return handler
  },
)

watch(
  () => props.input,
  () => grid.value?.gridApi?.refreshCells(),
)

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
    const checkAndWarn = (pasted: Result<{ rows: number; columns: number }>) => {
      if (
        pasted.ok &&
        (pasted.value.rows < data.length || pasted.value.columns < (data[0]?.length ?? 0))
      ) {
        pasteWarning.show(`Truncated pasted data to keep table within ${CELLS_LIMIT} limit`)
      }
    }
    const pasted = pasteFromClipboard(data, {
      rowIndex: focusedCell.rowIndex,
      colId: focusedCell.column.getColId(),
    })
    if (pasted instanceof Promise) pasted.then(checkAndWarn)
    else checkAndWarn(pasted)
  }
  return []
}

// === Column Default Definition ===

const headerComponentParams = proxyRefs({
  editedColId: computed(() =>
    editedCell.value?.rowIndex === 'header' ? editedCell.value.colKey : undefined,
  ),
  onHeaderEditingStarted: headerEventHandlers.headerEditingStarted,
  onHeaderEditingStopped: headerEventHandlers.headerEditingStopped,
})

const defaultColDef: ColDef<RowData> & {
  headerComponentParams: HeaderParams
} = {
  editable: true,
  resizable: true,
  sortable: false,
  lockPinned: true,
  menuTabs: ['generalMenuTab'],
  headerComponentParams,
  cellStyle: { 'padding-left': 0, 'border-right': '1px solid #C0C0C0' },
}
</script>

<script lang="ts">
export const widgetDefinition = defineWidget(
  WidgetInputIsSpecificMethodCall({
    module: ProjectPath.create('Standard.Table' as QualifiedName, 'Table' as QualifiedName),
    definedOnType: ProjectPath.create(
      'Standard.Table' as QualifiedName,
      'Table.Table' as QualifiedName,
    ),
    name: 'input' as Identifier,
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
  <div v-if="tree.expanded" class="WidgetTableEditor widgetExpanded">
    <ResizableWidget
      :input="input"
      metadataKey="WidgetTableEditor"
      :config="config"
      :updateCallback="updateCallback"
    >
      <AgGridTableView
        ref="grid"
        :defaultColDef="defaultColDef"
        :columnDefs="columnDefsTyped"
        :rowData="rowData"
        :getRowId="(row) => `${row.data.index}`"
        :components="{
          agColumnHeader: TableHeader,
        }"
        :stopEditingWhenCellsLoseFocus="true"
        :suppressDragLeaveHidesColumns="true"
        :suppressMoveWhenColumnDragging="true"
        :processDataFromClipboard="processDataFromClipboard"
        v-on="gridEventHandlers"
        @keydown.arrow-left.stop
        @keydown.arrow-right.stop
        @keydown.arrow-up.stop
        @keydown.arrow-down.stop
        @keydown.backspace.stop
        @keydown.delete.stop
        @pointerdown.stop
        @click.stop
        @columnMoved="onColumnMoved"
        @rowDragEnd="onRowDragEnd"
      />
    </ResizableWidget>
  </div>
  <div v-else class="WidgetTableEditor widgetSingleLine">
    <span class="collapsed widgetApplyPadding" v-text="collapsedText" />
  </div>
</template>

<style scoped>
.WidgetTableEditor.widgetExpanded {
  border-radius: var(--node-port-border-radius);
}

.collapsed {
  opacity: 0.7;
}

:deep(.newColumnCell) {
  display: none;
}

:deep(.rowIndexCell) {
  color: rgba(0, 0, 0, 0.4);
}

.WidgetTableEditor:deep(.ag-root-wrapper) {
  --ag-wrapper-border-radius: var(--node-port-border-radius);
  border: none;
}
</style>
