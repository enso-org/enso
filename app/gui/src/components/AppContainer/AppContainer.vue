<script lang="ts">
import { ModalWrapper as ModalWrapperReact } from '#/components/ModalWrapper'
import type { PaywallFeatureName } from '#/hooks/billing'
import { UserBar as UserBarReact } from '#/pages/dashboard/UserBar'
import CommandPalette from '$/components/CommandPalette.vue'
import { provideContainerData } from '$/providers/container'
import { useOpenedProjects, type Project } from '$/providers/openedProjects'
import { ContainerProviderForReact } from '$/providers/react/container'
import { provideRightPanelData } from '$/providers/rightPanel'
import { appContainerBindings } from '@/bindings'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import { useEvent } from '@/composables/events'
import ProjectView from '@/ProjectView.vue'
import { registerHandlers } from '@/providers/action'
import { provideAsyncResources } from '@/providers/asyncResources'
import { provideFullscreenRoot } from '@/providers/fullscreenRoot'
import { useGlobalEventRegistry } from '@/providers/globalEventRegistry'
import type { Icon } from '@/util/iconMetadata/iconName'
import { reactComponent } from '@/util/react'
import { BackendType, EnsoPath } from 'enso-common/src/services/Backend'
import { newDirectoryId, newProjectId } from 'enso-common/src/services/LocalBackend'
import * as objects from 'enso-common/src/utilities/data/object'
import { normalizeSlashes } from 'enso-common/src/utilities/file'
import { onMounted, onUnmounted, shallowRef, toRefs } from 'vue'
import { Drive, Settings } from './reactTabs'
import RightPanel from './RightPanel.vue'
import SelectableTab from './SelectableTab.vue'

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
const { tab, projectTabs } = toRefs(provideContainerData())
provideAsyncResources(openedProjects)
provideRightPanelData(tab, props.isFeatureUnderPaywall)
provideFullscreenRoot(fullscreenRoot)

openedProjects.syncWithLocalStorage()

function loadingProjectSpinnerPhase(project: Project) {
  return project.state.info.mode === 'cloud' ? 'loading-slow' : 'loading-fast'
}

function closeSettingsTab() {
  // The settings tab autohide when not selected.
  tab.value = 'drive'
}

function closeTab() {
  switch (tab.value) {
    case 'settings':
      closeSettingsTab()
      break
    case 'drive':
      break
    default: {
      const project = projectTabs.value.find((proj) => proj.state.info.ensoPath === tab.value)
      if (project) openedProjects.closeProject(project.state.info.id)
      break
    }
  }
}

const actionHandlers = registerHandlers({
  'app.closeTab': {
    action: closeTab,
  },
})

const keydownHandler = appContainerBindings.handler(
  objects.mapEntries(
    appContainerBindings.bindings,
    (actionName) => actionHandlers[actionName].action,
  ),
)

const { globalEventRegistry } = useGlobalEventRegistry()
useEvent(globalEventRegistry, 'keydown', (event) => {
  return keydownHandler(event)
})

function projectIcon(project: Project): Icon | undefined {
  if (project.error != null) {
    return 'error'
  }
  if (project.state.status === 'closed-by-backend') {
    return 'warning'
  }
  if (project.nextTask?.process === 'opening' || project.nextTask?.process === 'restoring') {
    return undefined
  }
  return 'graph_editor'
}

const onSignOut = () => {
  openedProjects.closeAllProjects()
}

onMounted(() => {
  window.api?.menu?.setMenuItemHandler('closeTab', closeTab)
  window.api?.projectManagement.setOpenProjectHandler((project) => {
    const projectId = newProjectId(normalizeSlashes(project.projectRoot))

    openedProjects.openProjectLocally(
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
        <div role="tablist" class="tablist">
          <SelectableTab
            :selected="tab === 'drive'"
            icon="drive"
            label="Data Catalog"
            @update:selected="$event && (tab = 'drive')"
          />
          <SelectableTab
            v-for="project in projectTabs"
            :key="project.state.info.id"
            data-testid="project-view-tab-button"
            :selected="project.shown.value"
            :icon="projectIcon(project)"
            :label="
              project.state.status === 'initialized' ?
                project.state.name.value
              : project.state.info.title
            "
            @update:selected="$event && (tab = project.state.info.ensoPath)"
            @close="openedProjects.closeProject(project.state.info.id)"
          >
            <GrowingSpinner
              v-if="
                project.nextTask?.process === 'opening' || project.nextTask?.process === 'restoring'
              "
              :phase="loadingProjectSpinnerPhase(project)"
              :size="16"
            />
          </SelectableTab>
          <SelectableTab
            v-if="tab === 'settings'"
            :selected="true"
            icon="settings"
            label="Settings"
            @close="closeSettingsTab"
          />
        </div>
        <UserBar :goToSettingsPage="() => (tab = 'settings')" @signOut="onSignOut" />
      </div>
      <div class="mainView">
        <div class="panel">
          <KeepAlive>
            <Drive v-if="tab === 'drive'" />
          </KeepAlive>
          <KeepAlive v-for="project in projectTabs" :key="project.state.info.id">
            <ProjectView v-if="project.shown.value" :projectId="project.state.info.id" />
          </KeepAlive>
          <KeepAlive>
            <Settings v-if="tab === 'settings'" />
          </KeepAlive>
        </div>
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
}

.bar {
  background-color: rgba(0, 0, 0, 0.1);
  display: flex;
  flex-direction: row;
  align-items: center;
  justify-content: space-between;
  height: 3rem;
  min-height: 3rem;
  position: relative;
  padding: 0 8px;
  z-index: 1;
}

.tablist {
  display: flex;
  flex-direction: row;
  /* Create a stacking context for tab highlight, so it's under all tabs' contents. */
  isolation: isolate;
  font-family: var(--font-sans);
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
