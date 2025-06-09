<script setup lang="ts">
import RightPanel from '$/components/AppContainer/RightPanel.vue'
import { provideBackends } from '$/providers/backends'
import { provideHttpClient } from '$/providers/httpClient'
import { provideOpenedProjects } from '$/providers/openedProjects'
import { ContextsForReactProvider } from '$/providers/react'
import { provideText } from '$/providers/text'
import ReactRoot from '$/ReactRoot'
import '@/assets/base.css'
import { interactionBindings } from '@/bindings'
import TooltipDisplayer from '@/components/TooltipDisplayer.vue'
import { useEvent } from '@/composables/events'
import ProjectView from '@/ProjectView.vue'
import { initializeActions } from '@/providers/action'
import { provideAppClassSet } from '@/providers/appClass'
import { provideFullscreenRoot } from '@/providers/fullscreenRoot'
import { provideGuiConfig } from '@/providers/guiConfig'
import { provideInteractionHandler } from '@/providers/interactionHandler'
import { provideKeyboard } from '@/providers/keyboard'
import { provideTooltipRegistry } from '@/providers/tooltipRegistry'
import { registerAutoBlurHandler, registerGlobalBlurHandler } from '@/util/autoBlur'
import { baseConfig, configValue, mergeConfig, type ApplicationConfigValue } from '@/util/config'
import { reactComponent } from '@/util/react'
import { urlParams } from '@/util/urlParams'
import { useQueryClient } from '@tanstack/vue-query'
import { Platform, platform } from 'enso-common/src/detect'
import { computed, onMounted, shallowRef } from 'vue'
import { ComponentProps } from 'vue-component-type-helpers'
import { provideContainerData } from './providers/container'
import { provideRightPanelData } from './providers/rightPanel'

const { projectViewOnly, onAuthenticated, rootDirPath } = defineProps<{
  // Used in Project View integration tests. Once both test projects will be merged, this should be
  // removed
  projectViewOnly?: { options: ComponentProps<typeof ProjectView> } | null
  onAuthenticated?: (accessToken: string | null) => void
  rootDirPath: string | undefined
}>()

const classSet = provideAppClassSet()
const appTooltips = provideTooltipRegistry()

const appConfig = computed(() =>
  mergeConfig(baseConfig, urlParams(), {
    onUnrecognizedOption: (p) => {
      const filtered = p.filter((p) => !p.startsWith('cloud-ide'))

      if (filtered.length > 0) {
        console.warn('Unrecognized option:', filtered)
      }
    },
  }),
)
const appConfigValue = computed((): ApplicationConfigValue => configValue(appConfig.value))

const ReactRootWrapper = reactComponent(ReactRoot)
const queryClient = useQueryClient()

provideKeyboard()
const textStore = provideText()
const config = provideGuiConfig(appConfigValue)
const interaction = provideInteractionHandler()
initializeActions()
registerAutoBlurHandler()
registerGlobalBlurHandler()

const interactionBindingsHandler = interactionBindings.handler({
  cancel: () => interaction.cancelAll(),
})

useEvent(window, 'keydown', interactionBindingsHandler)
useEvent(window, 'pointerdown', (e) => interaction.handlePointerDown(e), {
  capture: true,
})
const httpClient = provideHttpClient()
provideBackends(httpClient, config, rootDirPath, textStore.getText)

const platformClass = (() => {
  switch (platform()) {
    case Platform.windows:
      return 'onWindows'
    case Platform.macOS:
      return 'onMacOs'
    case Platform.linux:
      return 'onLinux'
    case Platform.windowsPhone:
      return 'onWindowsPhone'
    case Platform.iPhoneOS:
      return 'onIPhoneOs'
    case Platform.android:
      return 'onAndroid'
    default:
      return undefined
  }
})()

onMounted(() => {
  if (appConfigValue.value.window.vibrancy) {
    document.body.classList.add('vibrancy')
  }
})
const fullscreenRoot = shallowRef<HTMLElement>()

// Mock external context in Project View integration tests. Once both test projects will be merged,
// this should be removed
if (projectViewOnly) {
  provideOpenedProjects()
  provideContainerData([])
  provideRightPanelData(projectViewOnly.options.projectId, () => false, true, textStore)
  provideFullscreenRoot(fullscreenRoot)
}
</script>

<template>
  <div :class="['App', platformClass, ...classSet.keys()]">
    <div v-if="projectViewOnly" ref="fullscreenRoot" class="mainView">
      <ProjectView v-bind="projectViewOnly.options" />
      <RightPanel />
    </div>
    <ContextsForReactProvider v-else>
      <ReactRootWrapper
        :config="appConfigValue"
        :queryClient="queryClient"
        @authenticated="onAuthenticated ?? (() => {})"
      >
        <RouterView />
      </ReactRootWrapper>
    </ContextsForReactProvider>
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
  dominant-baseline: central;
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
  pointer-events: none;
  > * {
    pointer-events: auto;
  }
}

.mainView {
  flex-grow: 1;
  min-height: 0;
  display: flex;
  flex-direction: row;
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
