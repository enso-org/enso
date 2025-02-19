<script setup lang="ts">
import Backend from '#/services/Backend'
import GraphEditor from '@/components/GraphEditor.vue'
import { provideBackend } from '@/providers/backend'
import { provideEventLogger } from '@/providers/eventLogging'
import { provideVisibility } from '@/providers/visibility'
import { type LsUrls, provideProjectStore } from '@/stores/project'
import { provideProjectNames } from '@/stores/projectNames'
import { provideSettings } from '@/stores/settings'
import { type Opt } from '@/util/data/opt'
import { useEventListener } from '@vueuse/core'
import { markRaw, onActivated, onDeactivated, ref, toRaw, toRef, watch } from 'vue'

const props = defineProps<{
  readonly projectId: string
  readonly projectName: string
  readonly projectDisplayedName: string
  readonly projectNamespace?: string
  readonly engine: LsUrls
  readonly renameProject: (newName: string) => void
  /** The current project's backend, which may be remote or local. */
  readonly projectBackend?: Opt<Backend>
  /**
   * The remote backend.
   *
   * This is used regardless of whether the project is local for e.g. the cloud file browser.
   */
  readonly remoteBackend?: Opt<Backend>
}>()

provideBackend({
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

const projectNames = provideProjectNames(
  toRef(props, 'projectNamespace'),
  props.projectName,
  toRef(props, 'projectDisplayedName'),
)
provideProjectStore(props, projectNames)
provideSettings()

const visible = ref(false)
provideVisibility(visible)
onActivated(() => (visible.value = true))
onDeactivated(() => (visible.value = false))
</script>

<template>
  <div class="ProjectView">
    <GraphEditor />
  </div>
</template>

<style scoped>
.ProjectView {
  flex: 1;
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

:deep(*),
:deep(*)::before,
:deep(*)::after {
  box-sizing: border-box;
  margin: 0;
}

:deep(.icon) {
  width: 16px;
  height: 16px;
}

/* Scrollbar style definitions for textual visualizations which need support for scrolling.
 *
 * The 11px width/height (depending on scrollbar orientation)
 * is set so that it resembles macOS default scrollbar.
 */

:deep(.scrollable) {
  scrollbar-color: rgba(190 190 190 / 50%) transparent;
  &::-webkit-scrollbar {
    -webkit-appearance: none;
  }
  &::-webkit-scrollbar-track {
    -webkit-box-shadow: none;
  }
  &::-webkit-scrollbar:vertical {
    width: 11px;
  }
  &::-webkit-scrollbar:horizontal {
    height: 11px;
  }
  &::-webkit-scrollbar-thumb {
    border-radius: 8px;
    border: 1px solid rgba(220, 220, 220, 0.5);
    background-color: rgba(190, 190, 190, 0.5);
  }
  &::-webkit-scrollbar-corner {
    background: rgba(0, 0, 0, 0);
  }
  &::-webkit-scrollbar-button {
    height: 8px;
    width: 8px;
  }
}

:deep(.draggable) {
  cursor: grab;
}

:deep(.clickable) {
  cursor: pointer;
}
</style>
