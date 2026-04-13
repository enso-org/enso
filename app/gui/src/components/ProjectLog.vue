<script setup lang="ts">
import { useLogData } from '$/components/ProjectLog/projectLogData'
import ProjectLogTable from '$/components/ProjectLog/ProjectLogTable.vue'
import ProjectLogToolbar from '$/components/ProjectLog/ProjectLogToolbar.vue'
import type { ProjectLogTab } from '$/providers/container'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import { registerHandlers } from '@/providers/action'
import { computed } from 'vue'

const props = defineProps<{ tab: ProjectLogTab }>()

const sessionId = computed(() => props.tab.id)
const projectTitle = computed(() => props.tab.title)

const { logEntries, dataActions, isLoading, isFetchingNextPage } = useLogData({
  sessionId,
  projectTitle,
})

registerHandlers({
  'sessionLogs.reload': dataActions.reload,
  'sessionLogs.loadMore': dataActions.loadMore,
  'sessionLogs.download': dataActions.download,
})
</script>

<template>
  <div class="ProjectLog">
    <ProjectLogToolbar />
    <template v-if="!isLoading">
      <ProjectLogTable :entries="logEntries" />
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

.LoadingSpinner {
  text-align: center;
  width: 100%;
  margin-top: 4rem;
}
</style>
