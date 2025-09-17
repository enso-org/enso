<script setup lang="ts">
import Backend, { ProjectId } from '#/services/Backend'
import WithCurrentProject from '$/components/WithCurrentProject.vue'
import { injectOpenedProjects } from '$/providers/openedProjects'
import GraphEditor from '@/components/GraphEditor.vue'
import { provideEventLogger } from '@/providers/eventLogging'
import { provideProjectBackend } from '@/providers/projectBackend'
import { provideVisibility } from '@/providers/visibility'
import { type LsUrls } from '@/stores/project'
import { provideSettings } from '@/stores/settings'
import { type Opt } from '@/util/data/opt'
import { useEventListener } from '@vueuse/core'
import {
  markRaw,
  onActivated,
  onDeactivated,
  onMounted,
  onScopeDispose,
  ref,
  toRaw,
  toRef,
  toRefs,
  watch,
} from 'vue'

const props = defineProps<{
  readonly projectId: ProjectId
  readonly projectInitialName: string
  readonly projectDisplayedName: string
  readonly projectNamespace?: string
  readonly projectPath: string
  readonly engine: LsUrls
  readonly renameProject: (newName: string) => Promise<void>
  /** The current project's backend, which may be remote or local. */
  readonly projectBackend?: Opt<Backend>
  /**
   * The remote backend.
   *
   * This is used regardless of whether the project is local for e.g. the cloud file browser.
   */
  readonly remoteBackend?: Opt<Backend>
}>()

provideProjectBackend({
  project: () => (props.projectBackend && markRaw(toRaw(props.projectBackend))) ?? null,
  remote: () => (props.remoteBackend && markRaw(toRaw(props.remoteBackend))) ?? null,
})

const logger = provideEventLogger(
  ref((message: string, projectId?: string | null, metadata?: object | null) => {
    void props.remoteBackend?.logEvent(message, projectId, metadata)
  }),
  toRef(props, 'projectId'),
)
watch(
  toRef(props, 'projectId'),
  (_id, _oldId, onCleanup) => {
    logger.send('ide_project_opened')
    onCleanup(() => logger.send('ide_project_closed'))
  },
  { immediate: true },
)

useEventListener(window, 'beforeunload', () => logger.send('ide_project_closed'))

const openedProjects = injectOpenedProjects()
provideSettings()

const visible = ref(false)
provideVisibility(visible)
openedProjects.registerProject(toRefs(props))
onScopeDispose(() => openedProjects.unregisterProject(props.projectId))

onMounted(() => (visible.value = true))
onActivated(() => (visible.value = true))
onDeactivated(() => (visible.value = false))
</script>

<template>
  <div id="ProjectView" class="ProjectView">
    <WithCurrentProject :id="projectId">
      <!-- Key property is needed because of still many usages of deprecated useXStore 
       (see WithCurrentProject.vue). Once all those usages disappear, fully remouting GraphEditor
       will be no longer necessary -->
      <GraphEditor v-if="projectId" :key="projectId" />
    </WithCurrentProject>
  </div>
</template>

<style scoped>
.ProjectView {
  width: 100%;
  height: 100%;
  color: var(--color-text);
  font-family: var(--font-sans);
  font-weight: 500;
  font-size: 11.5px;
  line-height: 20px;
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  pointer-events: all;
  cursor: default;
}

:deep(.icon) {
  width: 16px;
  height: 16px;
}

:deep(.draggable) {
  cursor: grab;
}
</style>
