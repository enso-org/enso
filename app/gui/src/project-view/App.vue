<script setup lang="ts">
import HelpScreen from '@/components/HelpScreen.vue'
import { provideAppClassSet } from '@/providers/appClass'
import { provideBackend } from '@/providers/backend'
import { provideEventLogger } from '@/providers/eventLogging'
import { provideGuiConfig } from '@/providers/guiConfig'
import { registerAutoBlurHandler } from '@/util/autoBlur'
import {
  baseConfig,
  configValue,
  mergeConfig,
  type ApplicationConfigValue,
  type StringConfig,
} from '@/util/config'
import ProjectView from '@/views/ProjectView.vue'
import { useEventListener } from '@vueuse/core'
import type Backend from 'enso-common/src/services/Backend'
import { computed, markRaw, toRaw, toRef, watch } from 'vue'
import TooltipDisplayer from './components/TooltipDisplayer.vue'
import { provideTooltipRegistry } from './providers/tooltipState'
import { provideVisibility } from './providers/visibility'
import { urlParams } from './util/urlParams'

const props = defineProps<{
  config: StringConfig
  projectId: string
  logEvent: LogEvent
  hidden: boolean
  ignoreParamsRegex?: RegExp
  renameProject: (newName: string) => void
  /** The current project's backend, which may be remote or local. */
  projectBackend: Backend | null
  /**
   * The remote backend.
   *
   * This is used regardless of whether the project is local for e.g. the cloud file browser.
   */
  remoteBackend: Backend | null
}>()

provideBackend({
  project: () => props.projectBackend && markRaw(toRaw(props.projectBackend)),
  remote: () => props.remoteBackend && markRaw(toRaw(props.remoteBackend)),
})

const classSet = provideAppClassSet()
const appTooltips = provideTooltipRegistry()

const logger = provideEventLogger(toRef(props, 'logEvent'), toRef(props, 'projectId'))
watch(
  toRef(props, 'projectId'),
  (_id, _oldId, onCleanup) => {
    logger.send('ide_project_opened')
    onCleanup(() => logger.send('ide_project_closed'))
  },
  { immediate: true },
)

useEventListener(window, 'beforeunload', () => logger.send('ide_project_closed'))

const appConfig = computed(() => {
  const unrecognizedOptions: string[] = []
  const intermediateConfig = mergeConfig(
    baseConfig,
    urlParams({ ignoreKeysRegExp: props.ignoreParamsRegex }),
    {
      onUnrecognizedOption: (p) => unrecognizedOptions.push(p.join('.')),
    },
  )
  return {
    unrecognizedOptions,
    config: mergeConfig(intermediateConfig, props.config ?? {}),
  }
})

provideGuiConfig(computed((): ApplicationConfigValue => configValue(appConfig.value.config)))
provideVisibility(computed(() => !props.hidden))

registerAutoBlurHandler()
</script>

<template>
  <HelpScreen
    v-if="appConfig.unrecognizedOptions.length"
    v-show="!props.hidden"
    :unrecognizedOptions="appConfig.unrecognizedOptions"
    :config="appConfig.config"
  />
  <ProjectView
    v-else
    v-show="!props.hidden"
    v-bind="$attrs"
    class="App"
    :class="[...classSet.keys()]"
    :projectId="props.projectId"
    :renameProject="renameProject"
  />
  <Teleport to="body">
    <TooltipDisplayer :registry="appTooltips" />
  </Teleport>
</template>

<style scoped>
.App {
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

.enso-dashboard .App {
  /* Compensate for top bar, render the app below it. */
  top: calc(var(--row-height) + var(--top-level-gap, 0px) + var(--top-bar-margin, 0px) + 16px);
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

:deep([data-use-vue-component-wrap]) {
  display: contents !important;
}
</style>
