<script setup lang="ts">
import LoadingScreenReact from '#/pages/authentication/LoadingScreen'
import { useAppTitle } from '$/composables/appTitle'
import { useAuth } from '$/providers/auth'
import { ContextsForReactProvider } from '$/providers/react/globalProvider'
import ReactRoot from '$/ReactRoot'
import { appOpenCloseCallback } from '$/utils/analytics'
import '@/assets/base.css'
import { appBindings } from '@/bindings'
import TooltipDisplayer from '@/components/TooltipDisplayer.vue'
import { useEvent, useMounted } from '@/composables/events'
import { initializeActions, registerHandlers } from '@/providers/action'
import { provideAppClassSet } from '@/providers/appClass'
import { provideGlobalEventRegistry } from '@/providers/globalEventRegistry'
import { provideInteractionHandler } from '@/providers/interactionHandler'
import { provideBubblingKeyboard, provideKeyboard } from '@/providers/keyboard'
import { provideTooltipRegistry } from '@/providers/tooltipRegistry'
import { registerAutoBlurHandler, registerGlobalBlurHandler } from '@/util/autoBlur'
import { reactComponent } from '@/util/react'
import { useQueryClient } from '@tanstack/vue-query'
import * as objects from 'enso-common/src/utilities/data/object'
import { Platform, platform } from 'enso-common/src/utilities/detect'
import { computed } from 'vue'

const LoadingScreen = reactComponent(LoadingScreenReact)

const classSet = provideAppClassSet()
const appTooltips = provideTooltipRegistry()

const ReactRootWrapper = reactComponent(ReactRoot)
const queryClient = useQueryClient()

const auth = useAuth()
const userSession = computed(() => auth.session)

useAppTitle(userSession)
const globalEvents = provideGlobalEventRegistry()
provideKeyboard(globalEvents)
provideBubblingKeyboard(globalEvents)
const interaction = provideInteractionHandler()
const actions = initializeActions()
registerAutoBlurHandler()
registerGlobalBlurHandler()

const actionHandlers = registerHandlers(
  {
    'app.cancel': { action: () => interaction.cancelAll() },
    'app.close': { action: () => window.close() },
  },
  actions,
)

const bindingsHandlers = appBindings.handler(
  objects.mapEntries(appBindings.bindings, (actionName) => actionHandlers[actionName].action),
)

const { globalEventRegistry } = globalEvents
useEvent(globalEventRegistry, 'keydown', (event) => bindingsHandlers(event), { capture: true })

useEvent(globalEventRegistry, 'pointerdown', (e) => interaction.handlePointerDown(e), {
  capture: true,
})

const platformClass = {
  [Platform.windows]: 'onWindows',
  [Platform.macOS]: 'onMacOs',
  [Platform.linux]: 'onLinux',
  [Platform.windowsPhone]: 'onWindowsPhone',
  [Platform.iPhoneOS]: 'onIPhoneOs',
  [Platform.android]: 'onAndroid',
  [Platform.unknown]: undefined,
}[platform()]

useMounted(appOpenCloseCallback)
</script>

<template>
  <div :class="['App', platformClass, ...classSet.keys()]">
    <ContextsForReactProvider>
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
  /* This is to ensure the tooltips and floating elements will be over all other app elements */
  isolation: isolate;
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

.mousePointer {
  position: absolute;
  width: 20px;
  height: 20px;
  pointer-events: none;
  background-color: red;
}
</style>
