<script setup lang="ts">
import { useBackends } from '$/providers/backends'
import type { ProjectLogTab } from '$/providers/container'
import ActionButton from '@/components/ActionButton.vue'
import AgGridTableView from '@/components/AgGridTableView.vue'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import { registerHandlers } from '@/providers/action'
import * as vueQuery from '@tanstack/vue-query'
import { BackendType } from 'enso-common/src/services/Backend'
import { computed } from 'vue'
import type { ComponentProps } from 'vue-component-type-helpers'

// === Inputs ===

const props = defineProps<{ tab: ProjectLogTab }>()

const sessionId = computed(() => props.tab.id)
const projectTitle = computed(() => props.tab.title)
const backendType = computed(() =>
  props.tab.backend === 'remote' ? BackendType.remote : BackendType.local,
)

// === Fetching log data ===

const { backendForType } = useBackends()
const backend = computed(() => backendForType(backendType.value))

const logs = vueQuery.useInfiniteQuery({
  queryKey: [
    'projectLogs',
    { projectSessionId: sessionId.value, projectTitle: projectTitle.value, infinite: true },
  ],
  queryFn: ({ pageParam }) =>
    backend.value.getProjectSessionLogs(
      sessionId.value,
      { scrollId: pageParam },
      projectTitle.value,
    ),
  initialPageParam: null as string | null,
  getNextPageParam: (page) => (page.hits.length === 0 ? null : page.scrollId),
})
const logData = computed(() => logs.data.value?.pages.flatMap((page) => page.hits) ?? [])

// === Log data operations ===

const dataActions = {
  reload: {
    action: () => logs.refetch(),
  },
  loadMore: {
    action: () => logs.fetchNextPage(),
    enabled: () => logs.hasNextPage.value && !logs.isFetchingNextPage.value,
  },
  download: {
    action: () => backend.value.downloadProjectSessionLogs(sessionId.value),
  },
}

// === Log action bindings ===

registerHandlers({
  'sessionLogs.reload': dataActions.reload,
  'sessionLogs.loadMore': dataActions.loadMore,
  'sessionLogs.download': dataActions.download,
})

// === Table data definition ===

interface LogEntry {
  level: number
  timestamp: Date
  module: string
  event: string
}
type TableView = typeof AgGridTableView<LogEntry, unknown>

// === Log parsing ===

const logEntries = computed(() => parseEntries(logData.value))

const LOG_LEVELS = ['ERROR', 'WARN', 'INFO', 'DEBUG', 'TRACE']

const entryRe = /\[(TRACE|DEBUG|INFO|WARN|ERROR)] \[([^\]]+)] \[([^\]]+)] (.*)/
function parseEntry(line: string): LogEntry | undefined {
  const matched = entryRe.exec(line)
  if (!matched) return undefined
  const [_, levelName, timestamp, mod, event] = matched
  if (levelName == null || timestamp == null || mod == null || event == null) return undefined
  const level = LOG_LEVELS.findIndex((l) => l === levelName)
  if (level == null) return undefined
  return {
    level,
    timestamp: new Date(timestamp),
    module: mod,
    event,
  }
}

function parseEntries(lines: string[]): LogEntry[] {
  const result: LogEntry[] = []
  let current: LogEntry | undefined = undefined
  for (const line of lines) {
    const parsed = parseEntry(line)
    if (parsed) {
      if (current) result.push(current)
      current = parsed
    } else {
      if (current) {
        current.event += line
      } else {
        console.error('Invalid log entry', line)
      }
    }
  }
  if (current) result.push(current)
  return result
}

// === Log data formatting ===

function formatLogLevel({ value }: { value: unknown }): string {
  const num = value as number | null
  if (num == null) return ''
  return LOG_LEVELS[num] ?? ''
}

function formatTimestamp({ value }: { value: unknown }): string {
  const date = value as Date | null
  if (!date) return ''
  return date.toLocaleString(undefined, { dateStyle: 'short', timeStyle: 'medium' })
}

// === Table column configuration ===

// Note: This prop seems to be missing from the component type info, but it works as in the AG Grid docs.
const dataTypeDefinitions = {
  timestamp: {
    baseDataType: 'date',
    extendsDataType: 'date',
    valueFormatter: formatTimestamp,
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
    <template v-if="!logs.isLoading.value">
      <div class="logTable">
        <AgGridTableView
          :rowData="logEntries"
          :columnDefs="columnDefs"
          :defaultColDef="{}"
          :dataTypeDefinitions="dataTypeDefinitions"
        />
      </div>
    </template>
    <LoadingSpinner
      v-if="logs.isLoading.value || logs.isFetchingNextPage.value"
      phase="initial"
      :size="80"
    />
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
