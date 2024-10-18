<script lang="ts">
import { gridBindings } from '@/bindings'
import type { MenuItemDef } from 'ag-grid-enterprise'
/**
 * A more specialized version of AGGrid's `MenuItemDef` to simplify testing (the tests need to provide
 * only values actually used by the composable)
 */
export interface MenuItem<TData> extends MenuItemDef<TData> {
  action: (params: {
    node: { data: TData | undefined } | null
    api: { copyToClipboard: () => void; cutToClipboard: () => void; pasteFromClipboard: () => void }
  }) => void
}

const AGGRID_DEFAULT_COPY_ICON =
  '<span class="ag-icon ag-icon-copy" unselectable="on" role="presentation"></span>'

export const commonContextMenuActions = {
  cut: {
    name: 'Cut',
    shortcut: gridBindings.bindings['cutCells'].humanReadable,
    action: ({ api }) => api.cutToClipboard(),
    icon: AGGRID_DEFAULT_COPY_ICON,
  },
  copy: {
    name: 'Copy',
    shortcut: gridBindings.bindings['copyCells'].humanReadable,
    action: ({ api }) => api.copyToClipboard(),
    icon: AGGRID_DEFAULT_COPY_ICON,
  },
  paste: {
    name: 'Paste',
    shortcut: gridBindings.bindings['pasteCells'].humanReadable,
    action: ({ api }) => api.pasteFromClipboard(),
    icon: AGGRID_DEFAULT_COPY_ICON,
  },
} satisfies Record<string, MenuItem<unknown>>
</script>

<script setup lang="ts" generic="TData, TValue">
/**
 * Component adding some useful logic to AGGrid table component (like keeping track of colum sizes),
 * and using common style for tables in our application.
 */
import {
  clipboardNodeData,
  tsvTableToEnsoExpression,
  writeClipboard,
} from '@/components/GraphEditor/clipboard'
import { TextFormatOptions } from '@/components/visualizations/TableVisualization.vue'
import { useAutoBlur } from '@/util/autoBlur'
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
  ProcessDataFromClipboardParams,
  RowDataUpdatedEvent,
  RowEditingStartedEvent,
  RowEditingStoppedEvent,
  RowHeightParams,
  SortChangedEvent,
} from 'ag-grid-enterprise'
import { type ComponentInstance, reactive, ref, shallowRef, watch } from 'vue'

const DEFAULT_ROW_HEIGHT = 22

const _props = defineProps<{
  rowData: TData[]
  columnDefs: (ColDef<TData, TValue> | ColGroupDef<TData>)[] | null
  defaultColDef: ColDef<TData>
  getRowId?: GetRowIdFunc<TData>
  components?: Record<string, unknown>
  singleClickEdit?: boolean
  stopEditingWhenCellsLoseFocus?: boolean
  suppressDragLeaveHidesColumns?: boolean
  suppressMoveWhenColumnDragging?: boolean
  textFormatOption?: TextFormatOptions
  processDataFromClipboard?: (params: ProcessDataFromClipboardParams<TData>) => string[][] | null
}>()
const emit = defineEmits<{
  cellEditingStarted: [event: CellEditingStartedEvent]
  cellEditingStopped: [event: CellEditingStoppedEvent]
  rowEditingStarted: [event: RowEditingStartedEvent]
  rowEditingStopped: [event: RowEditingStoppedEvent]
  rowDataUpdated: [event: RowDataUpdatedEvent]
  sortOrFilterUpdated: [event: SortChangedEvent]
}>()

const widths = reactive(new Map<string, number>())
const wrapper = ref<HTMLElement>()
const grid = ref<ComponentInstance<typeof AgGridVue>>()
const gridApi = shallowRef<GridApi<TData>>()
const popupParent = document.body
useAutoBlur(() => grid.value?.$el)

function onGridReady(event: GridReadyEvent<TData>) {
  gridApi.value = event.api
}

function getRowHeight(params: RowHeightParams): number {
  if (_props.textFormatOption === 'off') {
    return DEFAULT_ROW_HEIGHT
  }
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

watch(
  () => _props.textFormatOption,
  () => {
    gridApi.value?.redrawRows()
    gridApi.value?.resetRowHeights()
  },
)

function updateColumnWidths(event: FirstDataRenderedEvent | RowDataUpdatedEvent) {
  if (event.api == null) {
    console.warn('AG Grid API does not exist.')
    return
  }
  const cols = event.api.getAllGridColumns().filter((c) => {
    const id = c.getColId()
    return id && !widths.has(id)
  })
  event.api.autoSizeColumns(cols)
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

/**
 * Copy the provided TSV-formatted table data to the clipboard.
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

// === Keybinds ===

const handler = gridBindings.handler({
  cutCells() {
    if (gridApi.value?.getFocusedCell() == null) return false
    gridApi.value?.cutToClipboard()
  },
  copyCells() {
    if (gridApi.value?.getFocusedCell() == null) return false
    gridApi.value?.copyToClipboard()
  },
  pasteCells() {
    if (gridApi.value?.getFocusedCell() == null) return false
    gridApi.value?.pasteFromClipboard()
  },
})

function supressCopy(event: KeyboardEvent) {
  // Suppress the default keybindings of AgGrid, because we want to use our own handlers (and bindings),
  // and AgGrid API does not allow copy suppression.
  if (
    (event.code === 'KeyX' || event.code === 'KeyC' || event.code === 'KeyV') &&
    event.ctrlKey &&
    wrapper.value != null &&
    event.target != wrapper.value
  ) {
    event.stopPropagation()
    wrapper.value.dispatchEvent(new KeyboardEvent(event.type, event))
  }
}

// === Loading AGGrid and its license ===

const { LicenseManager } = await import('ag-grid-enterprise')

if (typeof import.meta.env.VITE_ENSO_AG_GRID_LICENSE_KEY !== 'string') {
  console.warn('The AG_GRID_LICENSE_KEY is not defined.')
  if (import.meta.env.DEV) {
    // Hide annoying license validation errors in dev mode when the license is not defined. The
    // missing define warning is still displayed to not forget about it, but it isn't as obnoxious.
    const origValidateLicense = LicenseManager.prototype.validateLicense
    LicenseManager.prototype.validateLicense = function (this) {
      if (!('licenseManager' in this))
        Object.defineProperty(this, 'licenseManager', {
          configurable: true,
          set(value: any) {
            Object.getPrototypeOf(value).validateLicense = () => {}
            delete this.licenseManager
            this.licenseManager = value
          },
        })
      origValidateLicense.call(this)
    }
  }
} else {
  const agGridLicenseKey = import.meta.env.VITE_ENSO_AG_GRID_LICENSE_KEY
  LicenseManager.setLicenseKey(agGridLicenseKey)
}

const { AgGridVue } = await import('ag-grid-vue3')
</script>

<template>
  <div ref="wrapper" @keydown="handler" @keydown.capture="supressCopy">
    <AgGridVue
      v-bind="$attrs"
      ref="grid"
      class="ag-theme-alpine grid"
      :headerHeight="26"
      :getRowHeight="getRowHeight"
      :rowData="rowData"
      :columnDefs="columnDefs"
      :defaultColDef="defaultColDef"
      :sendToClipboard="sendToClipboard"
      :suppressFieldDotNotation="true"
      :enableRangeSelection="true"
      :popupParent="popupParent"
      :components="components"
      :singleClickEdit="singleClickEdit"
      :stopEditingWhenCellsLoseFocus="stopEditingWhenCellsLoseFocus"
      :suppressDragLeaveHidesColumns="suppressDragLeaveHidesColumns"
      :suppressMoveWhenColumnDragging="suppressMoveWhenColumnDragging"
      :processDataFromClipboard="processDataFromClipboard"
      @gridReady="onGridReady"
      @firstDataRendered="updateColumnWidths"
      @rowDataUpdated="updateColumnWidths($event), emit('rowDataUpdated', $event)"
      @columnResized="lockColumnSize"
      @cellEditingStarted="emit('cellEditingStarted', $event)"
      @cellEditingStopped="emit('cellEditingStopped', $event)"
      @rowEditingStarted="emit('rowEditingStarted', $event)"
      @rowEditingStopped="emit('rowEditingStopped', $event)"
      @sortChanged="emit('sortOrFilterUpdated', $event)"
      @filterChanged="emit('sortOrFilterUpdated', $event)"
    />
  </div>
</template>

<style src="@ag-grid-community/styles/ag-grid.css" />
<style src="@ag-grid-community/styles/ag-theme-alpine.css" />
<style scoped>
.grid {
  width: 100%;
  height: 100%;
}

.ag-theme-alpine {
  --ag-grid-size: 3px;
  --ag-list-item-height: 20px;
  font-family: var(--font-mono);
}

.TableVisualization > .ag-theme-alpine > :deep(.ag-root-wrapper.ag-layout-normal) {
  border-radius: 0 0 var(--radius-default) var(--radius-default);
}
</style>
