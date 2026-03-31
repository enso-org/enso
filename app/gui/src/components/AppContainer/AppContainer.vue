<script lang="ts">
import { ModalWrapper as ModalWrapperReact } from '#/components/ModalWrapper'
import { UserBar as UserBarReact } from '#/pages/dashboard/UserBar'
import CommandPalette from '$/components/CommandPalette.vue'
import { useContainerData } from '$/providers/container'
import { useOpenedProjects } from '$/providers/openedProjects'
import { ContainerProviderForReact } from '$/providers/react/container'
import { provideRightPanelData } from '$/providers/rightPanel'
import { appContainerBindings } from '@/bindings'
import { useEvent } from '@/composables/events'
import { registerHandlers, type ActionName } from '@/providers/action'
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

import type { TransferBetweenCategoriesFunction } from '#/layouts/Drive/Categories'
import type { ConfirmDeleteModalProps } from '#/modals/ConfirmDeleteModal'
import { provideDriveLocation } from '$/providers/drive'
import { provideReactApi } from '$/providers/reactApi'
import { useNavigateLink } from '$/utils/links'
import { proxyRefs } from '$/utils/reactivity'
import PopoverRootProvider from '@/components/PopoverRootProvider.vue'
import LeftPanel from './LeftPanel.vue'
import RightPanel from './RightPanel.vue'
import TabBar from './TabBar.vue'

const ModalWrapper = reactComponent(ModalWrapperReact)
const UserBar = reactComponent(UserBarReact)
</script>

<script setup lang="ts">
const props = defineProps<{
  startReactTransition: (action: () => void) => void
  isReactTransitioning: boolean
  transferBetweenCategories: TransferBetweenCategoriesFunction
  confirmDelete: (properties: ConfirmDeleteModalProps) => void
}>()

// NOTE: This cannot be `useTemplateRef`, because that creates a **readonly** ref, and it interferes
// with veaury's ref assignment implementation that runs during parent React component lifecycle.
const fullscreenRoot = shallowRef<HTMLElement>()

const openedProjects = useOpenedProjects()
const containerData = useContainerData()
const { openProjectLocally, openSettingsTab, closeCurrentTab } = containerData
const anyTabs = computed(() => containerData.tabList.length > 0)
provideAsyncResources(openedProjects)
provideRightPanelData(toRef(containerData, 'focusedPanel'))
provideFullscreenRoot(fullscreenRoot)
provideDriveLocation(props.startReactTransition)
provideReactApi(
  proxyRefs({
    startTransition: props.startReactTransition,
    isTransitioning: toRef(props, 'isReactTransitioning'),
    transferBetweenCategories: props.transferBetweenCategories,
    confirmDelete: props.confirmDelete,
  }),
)

const HELP_URLS: Record<ActionName & `help.${string}`, string> = {
  'help.whatsNew': 'https://community.ensoanalytics.com/c/what-is-new-in-enso/',
  'help.community': 'https://community.ensoanalytics.com/',
  'help.gettingStarted':
    'https://community.ensoanalytics.com/c/start-here/welcome-to-enso-community',
  'help.askAQuestion': 'https://community.ensoanalytics.com/c/q_and_a/',
  'help.componentExamples': 'https://community.ensoanalytics.com/c/enso-component-examples/',
  'help.exampleWorkflows': 'https://community.ensoanalytics.com/c/example-workflows/',
  'help.docs': 'https://help.enso.org/',
  'help.contactUs': 'https://ensoanalytics.com/contact',
}

const navigate = useNavigateLink()

const actionHandlers = registerHandlers({
  'app.closeTab': {
    action: closeCurrentTab,
  },
  ...objects.mapEntries(HELP_URLS, (key, value) => ({ action: () => navigate(value) })),
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
  <ContainerProviderForReact>
    <div class="AppContainer">
      <PopoverRootProvider>
        <div class="topBarBackground" />
        <CommandPalette />
        <ModalWrapper />
        <LeftPanel :middlePanelShown="anyTabs" :class="{ noMiddlePanel: !anyTabs }" />
        <div class="tabPanel" :class="{ noMiddlePanel: !anyTabs }">
          <div class="bar">
            <TabBar />
            <UserBar :goToSettingsPage="goToSettingsPage" @signOut="onSignOut" />
          </div>
          <div class="belowBar">
            <MiddlePanel v-if="anyTabs" />
            <RightPanel />
          </div>
        </div>
        <div ref="fullscreenRoot" class="FullscreenRoot" @wheel.stop />
      </PopoverRootProvider>
    </div>
  </ContainerProviderForReact>
</template>

<style scoped>
.AppContainer {
  --tab-highlight: var(--color-dashboard-background);
  --top-bar-height: 3rem;
  position: relative;
  display: flex;
  flex-direction: row;
  height: 100%;
  isolation: isolate;
}

.topBarBackground {
  position: absolute;
  width: 100%;
  height: var(--top-bar-height);
  background-color: rgba(0, 0, 0, 0.1);
}

.LeftPanel {
  flex-shrink: 0;

  &.noMiddlePanel {
    flex-grow: 1;
    flex-shrink: 1;
  }
}

.tabPanel {
  height: 100%;
  flex-grow: 1;
  min-width: 0;
  position: relative;
  display: flex;
  flex-direction: column;

  &.noMiddlePanel {
    flex-grow: 0;
  }
}

.bar {
  /* The bar should not contribute to "mainView" width when there's no middle panel. */
  position: absolute;
  width: 100%;
  display: flex;
  flex-direction: row;
  justify-content: right;
  height: var(--top-bar-height);
  min-height: var(--top-bar-height);
  padding: 0 8px 0 0;
  z-index: 1;
}

.TabBar {
  flex-grow: 1;
  min-width: 0;
}

.belowBar {
  margin-top: var(--top-bar-height);
  display: flex;
  flex-direction: row;
  flex-grow: 1;
  min-width: 0;
  min-height: 0;
}

.MiddlePanel {
  min-width: 0;
  flex-grow: 1;
}

.RightPanel {
  min-width: 48px;
  flex-shrink: 0;
}

.FullscreenRoot {
  position: absolute;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  pointer-events: none;
  z-index: 100;
  & > * {
    pointer-events: initial;
  }
}
</style>
