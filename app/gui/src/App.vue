<script setup lang="ts">
import '@/assets/base.css'
import TooltipDisplayer from '@/components/TooltipDisplayer.vue'
import ProjectView from '@/ProjectView.vue'
import { provideAppClassSet } from '@/providers/appClass'
import { provideGuiConfig } from '@/providers/guiConfig'
import { provideTooltipRegistry } from '@/providers/tooltipRegistry'
import { registerAutoBlurHandler, registerGlobalBlurHandler } from '@/util/autoBlur'
import { baseConfig, configValue, mergeConfig, type ApplicationConfigValue } from '@/util/config'
import { urlParams } from '@/util/urlParams'
import { useQueryClient } from '@tanstack/vue-query'
import { applyPureReactInVue } from 'veaury'
import { computed, onMounted } from 'vue'
import { ComponentProps } from 'vue-component-type-helpers'
import ReactRoot from './ReactRoot'

const _props = defineProps<{
  // Used in Project View integration tests. Once both test projects will be merged, this should be
  // removed
  projectViewOnly?: { options: ComponentProps<typeof ProjectView> } | null
  onAuthenticated?: (accessToken: string | null) => void
}>()

const classSet = provideAppClassSet()
const appTooltips = provideTooltipRegistry()

const appConfig = computed(() => {
  const config = mergeConfig(baseConfig, urlParams(), {
    onUnrecognizedOption: (p) => console.warn('Unrecognized option:', p),
  })
  return config
})
const appConfigValue = computed((): ApplicationConfigValue => configValue(appConfig.value))

const ReactRootWrapper = applyPureReactInVue(ReactRoot)
const queryClient = useQueryClient()

provideGuiConfig(appConfigValue)

registerAutoBlurHandler()
registerGlobalBlurHandler()

onMounted(() => {
  if (appConfigValue.value.window.vibrancy) {
    document.body.classList.add('vibrancy')
  }
})
</script>

<template>
  <div :class="['App', ...classSet.keys()]">
    <ProjectView v-if="projectViewOnly" v-bind="projectViewOnly.options" />
    <ReactRootWrapper
      v-else
      :config="appConfigValue"
      :queryClient="queryClient"
      @authenticated="onAuthenticated ?? (() => {})"
    />
  </div>
  <TooltipDisplayer :registry="appTooltips" />
</template>

<style>
.App {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
}

/*
TODO [ao]: Veaury adds a wrapping elements which have `style="all: unset"`, which in turn breaks our layout.
See https://github.com/gloriasoft/veaury/issues/158
*/
[__use_react_component_wrap],
[data-use-vue-component-wrap] {
  display: contents !important;
}
</style>
