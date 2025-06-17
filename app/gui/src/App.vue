<script setup lang="ts">
import LoadingScreenReact from '#/pages/authentication/LoadingScreen'
import RightPanel from '$/components/AppContainer/RightPanel.vue'
import { provideOpenedProjects } from '$/providers/openedProjects'
import { ContextsForReactProvider } from '$/providers/react/globalProvider'
import ReactRoot from '$/ReactRoot'
import '@/assets/base.css'
import { interactionBindings } from '@/bindings'
import TooltipDisplayer from '@/components/TooltipDisplayer.vue'
import { useEvent } from '@/composables/events'
import ProjectView from '@/ProjectView.vue'
import { initializeActions, registerHandlers } from '@/providers/action'
import { provideAppClassSet } from '@/providers/appClass'
import { provideFullscreenRoot } from '@/providers/fullscreenRoot'
import { provideGlobalEventRegistry } from '@/providers/globalEventRegistry'
import { injectGuiConfig } from '@/providers/guiConfig'
import { provideInteractionHandler } from '@/providers/interactionHandler'
import { provideKeyboard } from '@/providers/keyboard'
import { provideTooltipRegistry } from '@/providers/tooltipRegistry'
import { registerAutoBlurHandler, registerGlobalBlurHandler } from '@/util/autoBlur'
import { reactComponent } from '@/util/react'
import { useQueryClient } from '@tanstack/vue-query'
import { Platform, platform } from 'enso-common/src/detect'
import * as objects from 'enso-common/src/utilities/data/object'
import { onMounted, shallowRef } from 'vue'
import { ComponentProps } from 'vue-component-type-helpers'
import { provideContainerData } from './providers/container'
import { provideRightPanelData } from './providers/rightPanel'
import { useText } from './providers/text'

const { projectViewOnly } = defineProps<{
  // Used in Project View integration tests. Once both test projects will be merged, this should be
  // removed
  projectViewOnly?: { options: ComponentProps<typeof ProjectView> } | null
}>()

const LoadingScreen = reactComponent(LoadingScreenReact)

const config = injectGuiConfig()
const classSet = provideAppClassSet()
const appTooltips = provideTooltipRegistry()

const ReactRootWrapper = reactComponent(ReactRoot)
const queryClient = useQueryClient()

provideKeyboard()
const interaction = provideInteractionHandler()
const actions = initializeActions()
registerAutoBlurHandler()
registerGlobalBlurHandler()

const actionHandlers = registerHandlers(
  {
    'interaction.cancel': { action: () => interaction.cancelAll() },
  },
  actions,
)

const interactionBindingsHandler = interactionBindings.handler(
  objects.mapEntries(
    interactionBindings.bindings,
    (actionName) => actionHandlers[actionName].action,
  ),
)

const { globalEventRegistry } = provideGlobalEventRegistry()

useEvent(window, 'keydown', interactionBindingsHandler)
useEvent(globalEventRegistry, 'pointerdown', (e) => interaction.handlePointerDown(e))

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
  if (config.params.window.vibrancy) {
    document.body.classList.add('vibrancy')
  }
})
const fullscreenRoot = shallowRef<HTMLElement>()

// Mock external context in Project View integration tests. Once both test projects will be merged,
// this should be removed
if (projectViewOnly) {
  provideOpenedProjects()
  provideContainerData([])
  provideRightPanelData(projectViewOnly.options.projectId, () => false, true, useText())
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
      <ReactRootWrapper :queryClient="queryClient">
        <RouterView v-slot="{ Component }">
          <component :is="Component" v-if="Component" />
          <LoadingScreen v-else />
        </RouterView>
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
