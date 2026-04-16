<script setup lang="ts">
import { logLevelName, type LogEntry } from '$/components/ProjectLog/projectLogData'
import AgGridTableView from '@/components/AgGridTableView.vue'
import type { ComponentProps } from 'vue-component-type-helpers'

const { entries } = defineProps<{ entries: LogEntry[] }>()

type TableView = typeof AgGridTableView<LogEntry, unknown>

function formatDate({ value }: { value: unknown }): string {
  const date = value as Date | null
  if (date == null) return ''
  return date.toISOString()
}

function formatLogLevel({ value }: { value: unknown }): string {
  const num = value as number | null
  if (num == null) return ''
  return logLevelName(num)
}

// Note: This prop seems to be missing from the component type info, but it works as in the AG Grid docs.
const dataTypeDefinitions = {
  timestamp: {
    baseDataType: 'date',
    extendsDataType: 'date',
    valueFormatter: formatDate,
  },
  logLevel: {
    baseDataType: 'number',
    extendsDataType: 'number',
    valueFormatter: formatLogLevel,
  },
}

const columnDefs: ComponentProps<TableView>['columnDefs'] = [
  {
    field: 'level',
    cellDataType: 'logLevel',
    filter: 'agSetColumnFilter',
    filterParams: {
      valueFormatter: formatLogLevel,
      suppressSelectAll: true,
      suppressMiniFilter: true,
    },
  },
  {
    field: 'timestamp',
    cellDataType: 'timestamp',
  },
  {
    field: 'module',
    cellDataType: 'text',
    filter: 'agSetColumnFilter',
    filterParams: {
      treeList: true,
      treeListPathGetter: (data: string) => data.split('.'),
    },
  },
  { field: 'event', cellDataType: 'text' },
]
</script>

<template>
  <div class="ProjectLogTable">
    <AgGridTableView
      :rowData="entries"
      :columnDefs="columnDefs"
      :defaultColDef="{}"
      :dataTypeDefinitions="dataTypeDefinitions"
    />
  </div>
</template>

<style scoped>
.ProjectLogTable {
  height: 100%;
}
</style>
