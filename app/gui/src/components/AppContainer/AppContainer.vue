<script lang="ts">
import { ModalWrapper as ModalWrapperReact } from '#/components/ModalWrapper'
import type { PaywallFeatureName } from '#/hooks/billing'
import { UserBar as UserBarReact } from '#/pages/dashboard/UserBar'
import CommandPalette from '$/components/CommandPalette.vue'
import { useContainerData } from '$/providers/container'
import { useOpenedProjects } from '$/providers/openedProjects'
import { ContainerProviderForReact } from '$/providers/react/container'
import { provideRightPanelData } from '$/providers/rightPanel'
import { appContainerBindings } from '@/bindings'
import { useEvent } from '@/composables/events'
import { registerHandlers } from '@/providers/action'
import { provideAsyncResources } from '@/providers/asyncResources'
import { provideFullscreenRoot } from '@/providers/fullscreenRoot'
import { useGlobalEventRegistry } from '@/providers/globalEventRegistry'
import { reactComponent } from '@/util/react'
import { BackendType, EnsoPath } from 'enso-common/src/services/Backend'
import { newDirectoryId, newProjectId } from 'enso-common/src/services/LocalBackend'
import * as objects from 'enso-common/src/utilities/data/object'
import { normalizeSlashes } from 'enso-common/src/utilities/file'
import { computed, onMounted, onUnmounted, shallowRef, toRef } from 'vue'
import MiddlePanel from './MiddlePanel.vue'

import LeftPanel from './LeftPanel.vue'
import RightPanel from './RightPanel.vue'

const ModalWrapper = reactComponent(ModalWrapperReact)
const UserBar = reactComponent(UserBarReact)
</script>

<script setup lang="ts">
const props = defineProps<{
  isFeatureUnderPaywall(feature: PaywallFeatureName): boolean
}>()

// NOTE: This cannot be `useTemplateRef`, because that creates a **readonly** ref, and it interferes
// with veaury's ref assignment implementation that runs during parent React component lifecycle.
const fullscreenRoot = shallowRef<HTMLElement>()

const openedProjects = useOpenedProjects()
const containerData = useContainerData()
const { openProjectLocally, openSettingsTab, closeCurrentTab } = containerData
const anyTabs = computed(() => containerData.tabList.length > 0)
provideAsyncResources(openedProjects)
provideRightPanelData(toRef(containerData, 'focusedPanel'), props.isFeatureUnderPaywall)
provideFullscreenRoot(fullscreenRoot)

const actionHandlers = registerHandlers({
  'app.closeTab': {
    action: closeCurrentTab,
  },
})

const keydownHandler = appContainerBindings.handler(
  objects.mapEntries(
    appContainerBindings.bindings,
    (actionName) => actionHandlers[actionName].action,
  ),
)

function goToSettingsPage() {
  openSettingsTab()
}

const { globalEventRegistry } = useGlobalEventRegistry()
useEvent(globalEventRegistry, 'keydown', (event) => {
  return keydownHandler(event)
})

const onSignOut = () => {
  openedProjects.closeAllProjects()
}

onMounted(() => {
  window.api?.menu?.setMenuItemHandler('closeTab', closeCurrentTab)
  window.api?.projectManagement.setOpenProjectHandler((project) => {
    const projectId = newProjectId(normalizeSlashes(project.projectRoot))

    openProjectLocally(
      {
        id: projectId,
        title: project.name,
        parentId: newDirectoryId(normalizeSlashes(project.parentDirectory)),
        ensoPath: EnsoPath(String(normalizeSlashes(project.projectRoot))),
      },
      BackendType.local,
    )
  })
})

onUnmounted(() => {
  window.api?.projectManagement.setOpenProjectHandler(() => {})
})
</script>

<template>
  <div class="TabView">
    <CommandPalette />
    <ContainerProviderForReact>
      <ModalWrapper />
      <div class="bar">
        <UserBar :goToSettingsPage="goToSettingsPage" @signOut="onSignOut" />
      </div>
      <div class="mainView">
        <LeftPanel :middlePanelShown="anyTabs" />
        <MiddlePanel v-if="anyTabs" />
        <RightPanel />
        <div ref="fullscreenRoot" class="FullscreenRoot" @wheel.stop />
      </div>
    </ContainerProviderForReact>
  </div>
</template>

<style scoped>
.TabView {
  --tab-highlight: var(--color-dashboard-background);
  display: flex;
  flex-direction: column;
  height: 100%;
  isolation: isolate;
}

.bar {
  background-color: rgba(0, 0, 0, 0.1);
  display: flex;
  flex-direction: row-reverse;
  align-items: center;
  justify-content: space-between;
  height: 3rem;
  min-height: 3rem;
  position: relative;
  padding: 0 8px;
  z-index: 1;
}

.mainView {
  flex-grow: 1;
  min-height: 0;
  display: flex;
  flex-direction: row;
  position: relative;
}

.panel {
  flex-grow: 1;
  min-width: 0;
  display: flex;
  flex-direction: row;
}

.editor {
  display: contents;

  &.hidden {
    display: none;
  }
}

.FullscreenRoot {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  pointer-events: none;
  & > * {
    pointer-events: initial;
  }
}
</style>
