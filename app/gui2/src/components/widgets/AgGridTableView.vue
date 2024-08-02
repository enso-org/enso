<script setup lang="ts" generic="TData, TValue">
import {
  clipboardNodeData,
  tsvTableToEnsoExpression,
  writeClipboard,
} from '@/components/GraphEditor/clipboard'
import { useAutoBlur } from '@/util/autoBlur'
import '@ag-grid-community/styles/ag-grid.css'
import '@ag-grid-community/styles/ag-theme-alpine.css'
import type {
  CellEditingStartedEvent,
  CellEditingStoppedEvent,
  ColDef,
  ColGroupDef,
  ColumnResizedEvent,
  FirstDataRenderedEvent,
  GetRowIdFunc,
  GridApi,
  GridReadyEvent,
  RowDataUpdatedEvent,
  RowEditingStartedEvent,
  RowEditingStoppedEvent,
  RowHeightParams,
} from 'ag-grid-community'
import { AgGridVue } from 'ag-grid-vue3'
import { type ComponentInstance, reactive, ref, shallowRef } from 'vue'

const DEFAULT_ROW_HEIGHT = 22

const _props = defineProps<{
  rowData: TData[]
  columnDefs: (ColDef<TData, TValue> | ColGroupDef<TData>)[] | null
  defaultColDef: ColDef<TData>
  getRowId: GetRowIdFunc<TData>
}>()
const emit = defineEmits<{
  cellEditingStarted: [event: CellEditingStartedEvent]
  cellEditingStopped: [event: CellEditingStoppedEvent]
  rowEditingStarted: [event: RowEditingStartedEvent]
  rowEditingStopped: [event: RowEditingStoppedEvent]
}>()

const widths = reactive(new Map<string, number>())
const grid = ref<ComponentInstance<typeof AgGridVue>>()
const gridApi = shallowRef<GridApi<TData>>()
const popupParent = document.body
useAutoBlur(() => grid.value?.$el)

function onGridReady(event: GridReadyEvent) {
  gridApi.value = event.api
}

function getRowHeight(params: RowHeightParams): number {
  const rowData = Object.values(params.data)
  const textValues = rowData.filter((r): r is string => typeof r === 'string')

  if (!textValues.length) {
    return DEFAULT_ROW_HEIGHT
  }

  const returnCharsCount = textValues.map((text: string) => {
    const crlfCount = (text.match(/\r\n/g) || []).length
    const crCount = (text.match(/\r/g) || []).length
    const lfCount = (text.match(/\n/g) || []).length
    return crCount + lfCount - crlfCount
  })

  const maxReturnCharsCount = Math.max(...returnCharsCount)
  return (maxReturnCharsCount + 1) * DEFAULT_ROW_HEIGHT
}

function updateColumnWidths(event: FirstDataRenderedEvent | RowDataUpdatedEvent) {
  if (event.columnApi == null) {
    console.warn('AG Grid column API does not exist.')
    return
  }
  const cols = event.columnApi.getAllGridColumns().filter((c) => {
    const id = c.getColDef().colId
    return id && !widths.has(id)
  })
  console.log(cols)
  event.columnApi.autoSizeColumns(cols)
}

function lockColumnSize(e: ColumnResizedEvent) {
  // Check if the resize is finished, and it's not from the API (which is triggered by us).
  if (!e.finished || e.source === 'api') return
  // If the user manually resized (or manually autosized) a column, we don't want to auto-size it
  // on a resize.
  if (e.source !== 'autosizeColumns') {
    for (const column of e.columns ?? []) {
      const id = column.getColDef().colId
      if (id) widths.set(id, column.getActualWidth())
    }
  }
}

/** Copy the provided TSV-formatted table data to the clipboard.
 *
 * The data will be copied as `text/plain` TSV data for spreadsheet applications, and an Enso-specific MIME section for
 * pasting as a new table node.
 *
 * By default, AG Grid writes only `text/plain` TSV data to the clipboard. This is sufficient to paste into spreadsheet
 * applications, which are liberal in what they try to interpret as tabular data; however, when pasting into Enso, the
 * application needs to be able to distinguish tabular clipboard contents to choose the correct paste action.
 *
 * Our heuristic to identify clipboard data from applications like Excel and Google Sheets is to check for a <table> tag
 * in the clipboard `text/html` data. If we were to add a `text/html` section to the data so that it could be recognized
 * like other spreadsheets, when pasting into other applications some applications might use the `text/html` data in
 * preference to the `text/plain` content--so we would need to construct an HTML table that fully represents the
 * content.
 *
 * To avoid that complexity, we bypass our table-data detection by including application-specific data in the clipboard
 * content. This data contains a ready-to-paste node that constructs an Enso table from the provided TSV.
 */
function sendToClipboard({ data }: { data: string }) {
  return writeClipboard({
    ...clipboardNodeData([{ expression: tsvTableToEnsoExpression(data) }]),
    'text/plain': data,
  })
}

defineExpose({ gridApi })
</script>

<template>
  <AgGridVue
    ref="grid"
    class="grid ag-theme-alpine"
    :headerHeight="26"
    :getRowHeight="getRowHeight"
    :rowData="rowData"
    :columnDefs="columnDefs"
    :defaultColDef="defaultColDef"
    :sendToClipboard="sendToClipboard"
    :copyHeadersToClipboard="true"
    :suppressFieldDotNotation="true"
    :enableRangeSelection="true"
    :popupParent="popupParent"
    @gridReady="onGridReady"
    @firstDataRendered="updateColumnWidths"
    @rowDataUpdated="updateColumnWidths"
    @columnResized="lockColumnSize"
    @cellEditingStarted="emit('cellEditingStarted', $event)"
    @cellEditingStopped="emit('cellEditingStopped', $event)"
    @rowEditingStarted="emit('rowEditingStarted', $event)"
    @rowEditingStopped="emit('rowEditingStopped', $event)"
  />
</template>

<style scoped>
.ag-theme-alpine {
  --ag-grid-size: 3px;
  --ag-list-item-height: 20px;
  font-family: var(--font-mono);
}

.TableVisualization > .ag-theme-alpine > :deep(.ag-root-wrapper.ag-layout-normal) {
  border-radius: 0 0 var(--radius-default) var(--radius-default);
}
</style>
