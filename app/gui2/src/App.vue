<script setup lang="ts">
import HelpScreen from '@/components/HelpScreen.vue'
import { provideAppClassSet } from '@/providers/appClass'
import { provideGuiConfig } from '@/providers/guiConfig'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import { configValue, type ApplicationConfig, type ApplicationConfigValue } from '@/util/config'
import ProjectView from '@/views/ProjectView.vue'
import { isDevMode } from 'shared/util/detect'
import { computed, onMounted, toRaw } from 'vue'

const props = defineProps<{
  config: ApplicationConfig
  accessToken: string | null
  metadata: object
  unrecognizedOptions: string[]
}>()

const classSet = provideAppClassSet()

provideGuiConfig(computed((): ApplicationConfigValue => configValue(props.config)))

// Initialize suggestion db immediately, so it will be ready when user needs it.
onMounted(() => {
  const suggestionDb = useSuggestionDbStore()
  if (isDevMode) {
    ;(window as any).suggestionDb = toRaw(suggestionDb.entries)
  }
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
