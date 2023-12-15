<script setup lang="ts">
import DeprecatedVersionDialog from '@/components/DeprecatedVersionDialog.vue'
import HelpScreen from '@/components/HelpScreen.vue'
import { provideAppClassSet } from '@/providers/appClass'
import { provideGuiConfig, type GuiConfig } from '@/providers/guiConfig'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import type { ApplicationConfig } from '@/util/config'
import ProjectView from '@/views/ProjectView.vue'
import { onMounted, toRef } from 'vue'

const props = defineProps<{
  config: GuiConfig
  accessToken: string
  metadata: object
  unrecognizedOptions: string[]
  applicationConfig: ApplicationConfig
}>()

const classSet = provideAppClassSet()

provideGuiConfig(toRef(props, 'config'))

// Initialize suggestion db immediately, so it will be ready when user needs it.
onMounted(() => useSuggestionDbStore())
</script>

<template>
  <DeprecatedVersionDialog v-if="config.isVersionDeprecated === 'true'" />
  <HelpScreen
    v-if="unrecognizedOptions.length"
    :unrecognizedOptions="props.unrecognizedOptions"
    :applicationConfig="props.applicationConfig"
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
