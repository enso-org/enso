<script setup lang="ts">
import { useCurrentProject } from '$/components/WithCurrentProject.vue'
import { defineWidget, Score, widgetProps } from '$/providers/openedProjects/widgetRegistry'
import { WidgetEditHandler } from '$/providers/openedProjects/widgetRegistry/editHandler'
import { WidgetInputIsSpecificMethodCall } from '@/components/GraphEditor/widgets/WidgetFunction.vue'
import {
  CELLS_LIMIT,
  type RowData,
  tableInputCallMayBeHandled,
  useTableInputArgument,
} from '@/components/GraphEditor/widgets/WidgetTableEditor/tableInputArgument'
import AgGridTableView from '@/components/shared/AgGridTableView.vue'
import { targetIsOutside } from '@/util/autoBlur'
import { ProjectPath } from '@/util/projectPath'
import type { Identifier, QualifiedName } from '@/util/qualifiedName'
import { proxyRefs } from '@/util/reactivity'
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
import ResizableWidget from '../ResizableWidget.vue'
import TableHeader, { type HeaderParams } from './WidgetTableEditor/TableHeader.vue'
import { useTableEditHandler } from './WidgetTableEditor/editHandler'

const props = defineProps(widgetProps(widgetDefinition))
const { suggestionDb, module } = useCurrentProject()
const grid = ref<
  ComponentInstance<typeof AgGridTableView<RowData, any>> &
    ComponentExposed<typeof AgGridTableView<RowData, any>>
>()
const pasteWarning = useToast.warning()

const configSchema = z.object({
  size: z.object({ x: z.number(), y: z.number() }),
})
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
  module,
  () => suggestionDb.value.entries,
  props.updateCallback,
)

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

// === Resizing ===

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
  <div class="WidgetTableEditor widgetExpanded">
    <ResizableWidget
      :input="input"
      metadataKey="WidgetTableEditor"
      :config="config"
      :updateCallback="updateCallback"
    >
      <Suspense>
        <AgGridTableView
          ref="grid"
          class="inner"
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
      </Suspense>
    </ResizableWidget>
  </div>
</template>

<style scoped>
.WidgetTableEditor {
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
