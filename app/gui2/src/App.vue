<script setup lang="ts">
import HelpScreen from '@/components/HelpScreen.vue'
import { provideAppClassSet } from '@/providers/appClass'
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
import { computed, toRef, watch } from 'vue'
import { initializePrefixes } from './util/ast/node'
import { urlParams } from './util/urlParams'

const props = defineProps<{
  config: StringConfig
  projectId: string
  logEvent: LogEvent
  hidden: boolean
  ignoreParamsRegex?: RegExp
}>()

const classSet = provideAppClassSet()

initializePrefixes()

const logger = provideEventLogger(toRef(props, 'logEvent'), toRef(props, 'projectId'))
watch(
  [toRef(props, 'projectId')],
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

registerAutoBlurHandler()
</script>

<template>
  <Teleport to="#app">
    <HelpScreen
      v-if="appConfig.unrecognizedOptions.length"
      v-show="!props.hidden"
      :unrecognizedOptions="appConfig.unrecognizedOptions"
      :config="appConfig.config"
    />
    <ProjectView v-else v-show="!props.hidden" class="App" :class="[...classSet.keys()]" />
  </Teleport>
</template>

<style scoped>
.App {
  flex: 1;
  color: var(--color-text);
  font-family: var(--font-sans);
  font-size: 11.5px;
  font-weight: 500;
  line-height: 20px;
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
</style>
