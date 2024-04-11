<script setup lang="ts">
import HelpScreen from '@/components/HelpScreen.vue'
import { provideAppClassSet } from '@/providers/appClass'
import { provideGuiConfig } from '@/providers/guiConfig'
import { useProjectStore } from '@/stores/project'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { registerAutoBlurHandler } from '@/util/autoBlur'
import { configValue, type ApplicationConfig, type ApplicationConfigValue } from '@/util/config'
import ProjectView from '@/views/ProjectView.vue'
import { isDevMode } from 'shared/util/detect'
import { computed, onMounted, onUnmounted, toRaw } from 'vue'

const props = defineProps<{
  config: ApplicationConfig
  accessToken: string | null
  unrecognizedOptions: string[]
}>()

const classSet = provideAppClassSet()

provideGuiConfig(computed((): ApplicationConfigValue => configValue(props.config)))

registerAutoBlurHandler()

// Initialize suggestion db immediately, so it will be ready when user needs it.
onMounted(() => {
  const suggestionDb = useSuggestionDbStore()
  if (isDevMode) {
    ;(window as any).suggestionDb = toRaw(suggestionDb.entries)
  }
})
onUnmounted(() => {
  useProjectStore().disposeYDocsProvider()
})
</script>

<template>
  <HelpScreen
    v-if="unrecognizedOptions.length"
    :unrecognizedOptions="props.unrecognizedOptions"
    :config="props.config"
  />
  <ProjectView v-else class="App" :class="[...classSet.keys()]" />
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
