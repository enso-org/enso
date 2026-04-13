<script setup lang="ts">
import { logLevelName, useLogData, type LogEntry } from '$/components/projectLogData'
import type { ProjectLogTab } from '$/providers/container'
import ActionButton from '@/components/ActionButton.vue'
import AgGridTableView from '@/components/AgGridTableView.vue'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import { registerHandlers } from '@/providers/action'
import { computed } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'

// === Inputs ===

const props = defineProps<{ tab: ProjectLogTab }>()

const sessionId = computed(() => props.tab.id)
const projectTitle = computed(() => props.tab.title)

// === Fetching log data ===

const { logEntries, dataActions, isLoading, isFetchingNextPage } = useLogData({
  sessionId,
  projectTitle,
})

// === Log action bindings ===

registerHandlers({
  'sessionLogs.reload': dataActions.reload,
  'sessionLogs.loadMore': dataActions.loadMore,
  'sessionLogs.download': dataActions.download,
})

// === Table column configuration ===

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
  <div class="ProjectLog">
    <div class="buttons">
      <ActionButton action="sessionLogs.reload" />
      <ActionButton action="sessionLogs.loadMore" />
      <ActionButton action="sessionLogs.download" />
    </div>
    <template v-if="!isLoading">
      <div class="logTable">
        <AgGridTableView
          :rowData="logEntries"
          :columnDefs="columnDefs"
          :defaultColDef="{}"
          :dataTypeDefinitions="dataTypeDefinitions"
        />
      </div>
    </template>
    <LoadingSpinner v-if="isLoading || isFetchingNextPage" phase="initial" :size="80" />
  </div>
</template>

<style scoped>
.ProjectLog {
  padding: 1rem;
  max-height: 100%;
  max-width: 100%;
  overflow-y: auto;
  overflow-x: clip;

  font-family: monospace;

  height: 100%;
}

.logTable {
  height: 100%;
}

.buttons {
  display: flex;
  justify-content: flex-end;
  gap: 4px;
}

.LoadingSpinner {
  text-align: center;
  width: 100%;
  margin-top: 4rem;
}
</style>
