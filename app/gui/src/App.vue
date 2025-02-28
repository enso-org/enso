<script setup lang="ts">
import '@/assets/base.css'
import TooltipDisplayer from '@/components/TooltipDisplayer.vue'
import ProjectView from '@/ProjectView.vue'
import { initializeActions } from '@/providers/action'
import { provideAppClassSet } from '@/providers/appClass'
import { provideGuiConfig } from '@/providers/guiConfig'
import { provideInteractionHandler } from '@/providers/interactionHandler'
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
    onUnrecognizedOption: (p) => {
      const filtered = p.filter((p) => !p.startsWith('cloud-ide'))

      if (filtered.length > 0) {
        console.warn('Unrecognized option:', filtered)
      }
    },
  })
  return config
})
const appConfigValue = computed((): ApplicationConfigValue => configValue(appConfig.value))

const ReactRootWrapper = applyPureReactInVue(ReactRoot)
const queryClient = useQueryClient()

provideGuiConfig(appConfigValue)
provideInteractionHandler()
initializeActions()

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
  <div id="floatingLayer" />
  <TooltipDisplayer :registry="appTooltips" />
</template>

<style>
.App {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
}

#floatingLayer {
  position: absolute;
  color: var(--color-text);
  font-family: var(--font-sans);
  font-weight: 500;
  font-size: 11.5px;
  line-height: 20px;
  text-rendering: optimizeLegibility;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  top: 0;
  left: 0;
  /* The size isn't important, except it must be non-zero for `floating-ui` to calculate the scale factor. */
  width: 1px;
  height: 1px;
  contain: layout size style;
  will-change: transform;
  pointer-events: none;
  > * {
    pointer-events: auto;
  }
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
