<script lang="ts">
import type { PaywallFeatureName } from '#/hooks/billing'
import { UserBar as UserBarReact } from '#/pages/dashboard/UserBar'
import { BackendType, EnsoPath, type ProjectId } from '#/services/Backend'
import {
  useContainerData,
  type LaunchedProject,
  type OpenedProject,
  type TabId,
} from '$/providers/container'
import { RightPanelDataProviderForReact } from '$/providers/react/container'
import { provideRightPanelData } from '$/providers/rightPanel'
import { appContainerBindings } from '@/bindings'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import { useEvent } from '@/composables/events'
import { registerHandlers } from '@/providers/action'
import { provideFullscreenRoot } from '@/providers/fullscreenRoot'
import { reactComponent } from '@/util/react'
import * as objects from 'enso-common/src/utilities/data/object'
import { onMounted, reactive, shallowRef, toRefs, watch } from 'vue'
import { Drive, Editor, Settings } from './reactTabs'
import RightPanel from './RightPanel.vue'
import SelectableTab from './SelectableTab.vue'

const UserBar = reactComponent(UserBarReact)

/**
 * A part of `AppContainer` which needs some hooks passed from react by `Dashboard.tsx`.
 *
 * Should be merged back to AppContainer once all needed features will be moved to Vue store.
 */
export default {}
</script>

<script setup lang="ts">
const props = defineProps<{
  isFeatureUnderPaywall(feature: PaywallFeatureName): boolean
}>()

const emit = defineEmits<{
  closeProject: [project: LaunchedProject]
  closeAllProjects: []
}>()

// NOTE: This cannot be `useTemplateRef`, because that creates a **readonly** ref, and it interferes
// with veaury's ref assignment implementation that runs during parent React component lifecycle.
const fullscreenRoot = shallowRef<HTMLElement>()

const { tab, openingProjects, openedProjects } = toRefs(useContainerData())
provideRightPanelData(tab, props.isFeatureUnderPaywall)
provideFullscreenRoot(fullscreenRoot)

const readyProjects = reactive(new Set<ProjectId>())
const projectNames = reactive(new Map<ProjectId, string>())

function setProjectReady(project: ProjectId, projectTab: TabId, ready: boolean) {
  if (ready) {
    readyProjects.add(project)
    tab.value = projectTab
  } else {
    readyProjects.delete(project)
  }
}

function loadingProjectSpinnerPhase(project: OpenedProject) {
  return (
      project.state === 'launched' && (project.hybrid != null || project.type === BackendType.local)
    ) ?
      'loading-fast'
    : 'loading-slow'
}

function closeOpenedProject(project: OpenedProject) {
  if (project.state === 'launched') {
    emit('closeProject', project)
  } else {
    openingProjects.value.delete(project.id)
  }
}

watch(openedProjects, (openedProjectsList) => {
  const openedProjectsSet = new Set(openedProjectsList.map((proj) => proj.id))
  for (const proj of readyProjects) {
    if (!openedProjectsSet.has(proj)) {
      readyProjects.delete(proj)
    }
  }
  for (const proj of projectNames.keys()) {
    if (!openedProjectsSet.has(proj)) {
      projectNames.delete(proj)
    }
  }
})

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
      // project id
      const project = openedProjects.value.find((proj) => proj.ensoPath === tab.value)
      if (project) closeOpenedProject(project)
      break
    }
  }
}

onMounted(() => {
  window.menuApi?.setMenuItemHandler('closeTab', closeTab)
})

const actionHandlers = registerHandlers({
  'app.closeTab': {
    action: closeTab,
  },
})

useEvent(
  window,
  'keydown',
  appContainerBindings.handler(
    objects.mapEntries(
      appContainerBindings.bindings,
      (actionName) => actionHandlers[actionName].action,
    ),
  ),
)

const onSignOut = () => {
  emit('closeAllProjects')
}
</script>

<template>
  <RightPanelDataProviderForReact>
    <div class="bar">
      <div role="tablist" class="tablist">
        <SelectableTab
          :selected="tab === 'drive'"
          icon="drive"
          label="Data Catalog"
          @update:selected="$event && (tab = 'drive')"
        />
        <SelectableTab
          v-for="project in openedProjects"
          :key="project.id"
          data-testid="editor-tab-button"
          :selected="project.shown.value"
          :icon="readyProjects.has(project.id) ? 'graph_editor' : undefined"
          :label="projectNames.get(project.id)"
          @update:selected="$event && (tab = EnsoPath(project.ensoPath))"
          @close="closeOpenedProject(project)"
        >
          <GrowingSpinner
            v-if="!readyProjects.has(project.id)"
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
      <div class="filler" />
      <UserBar :goToSettingsPage="() => (tab = 'settings')" @signOut="onSignOut" />
    </div>
    <div class="mainView">
      <div class="panel">
        <KeepAlive>
          <Drive v-if="tab === 'drive'" />
        </KeepAlive>
        <template v-for="project in openedProjects">
          <div
            v-if="project.state === 'launched'"
            :key="project.id"
            class="editor"
            :class="{ hidden: !project.shown.value }"
          >
            <Editor
              :hidden="!project.shown.value"
              :project="project"
              @readyUpdate="setProjectReady(project.id, EnsoPath(project.ensoPath), $event)"
              @nameUpdate="projectNames.set(project.id, $event)"
            />
          </div>
        </template>

        <KeepAlive>
          <Settings v-if="tab === 'settings'" />
        </KeepAlive>
      </div>
      <RightPanel />
      <div ref="fullscreenRoot" class="FullscreenRoot" @wheel.stop />
    </div>
  </RightPanelDataProviderForReact>
</template>

<style scoped>
.bar {
  background-color: rgba(0, 0, 0, 0.1);
  display: flex;
  flex-direction: row;
  align-items: center;
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

.filler {
  flex-grow: 1;
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
