<script setup lang="ts">
import DeprecatedVersionDialog from '@/components/DeprecatedVersionDialog.vue'
import HelpScreen from '@/components/HelpScreen.vue'
import type { HelpInfo } from '@/components/HelpScreen/types'
import { provideAppClassSet } from '@/providers/appClass'
import { provideGuiConfig, type GuiConfig } from '@/providers/guiConfig'
import { useSuggestionDbStore } from '@/stores/suggestionDatabase'
import ProjectView from '@/views/ProjectView.vue'
import { onMounted, toRef } from 'vue'

const props = defineProps<{
  config: GuiConfig
  metadata: object
  helpInfo: HelpInfo
}>()

const classSet = provideAppClassSet()

provideGuiConfig(toRef(props, 'config'))

// Initialize suggestion db immediately, so it will be ready when user needs it.
onMounted(() => useSuggestionDbStore())
</script>

<template>
  <DeprecatedVersionDialog v-if="config.isVersionDeprecated === 'true'" />
  <HelpScreen v-if="props.helpInfo" v-bind="props.helpInfo" />
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
