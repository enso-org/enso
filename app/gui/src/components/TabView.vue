<script lang="ts">
import UserBarReact from '#/layouts/UserBar'
import { LaunchedProject } from '#/providers/ProjectsProvider'
import { BackendType, ProjectId } from '#/services/Backend'
import { Drive, Editor, Settings } from '$/components/TabView/reactTabs'
import RightPanel from '$/components/TabView/RightPanel.vue'
import SelectableTab from '$/components/TabView/SelectableTab.vue'
import { provideContainerData } from '$/providers/container'
import { provideOpenedProjects } from '$/providers/openedProjects'
import { RightPanelDataProviderForReact } from '$/providers/react'
import { provideRightPanelData } from '$/providers/rightPanel'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import { applyPureReactInVue } from 'veaury'
import { reactive, toRefs, watch } from 'vue'

const UserBar = applyPureReactInVue(UserBarReact)
</script>

<script setup lang="ts">
const { initialProjectName, launchedProjects, closeProject, closeAllProjects } = defineProps<{
  initialProjectName: string | null
  launchedProjects: readonly LaunchedProject[]
  closeProject(project: LaunchedProject): void
  closeAllProjects(): void
}>()

provideOpenedProjects()
const { tab, openedProjects } = toRefs(provideContainerData(() => launchedProjects))
provideRightPanelData(tab)

const readyProjects = reactive(new Set<ProjectId>())
const projectNames = reactive(new Map<ProjectId, string>())

function setProjectReady(project: ProjectId, ready: boolean) {
  if (ready) {
    readyProjects.add(project)
    tab.value = project
  } else {
    readyProjects.delete(project)
  }
}

function loadingProjectSpinnerPhase(project: LaunchedProject) {
  return project.hybrid != null || project.type === BackendType.local ?
      'loading-fast'
    : 'loading-slow'
}

watch(
  () => openedProjects,
  () => {
    const openedProjectsSet = new Set(openedProjects.value.map((proj) => proj.id))
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
  },
)

const onSignOut = () => {
  tab.value = 'drive'
  void closeAllProjects()
}
</script>
<template>
  <div class="TabView">
    <RightPanelDataProviderForReact>
      <div class="bar">
        <div role="tablist" class="tablist">
          <SelectableTab
            layoutId="tab-highlight"
            :selected="tab === 'drive'"
            icon="drive"
            label="Data Catalog"
            @update:selected="$event && (tab = 'drive')"
          />
          <SelectableTab
            v-for="project in launchedProjects"
            :key="project.id"
            data-testid="editor-tab-button"
            layoutId="tab-highlight"
            :selected="tab === project.id"
            :icon="readyProjects.has(project.id) ? 'graph_editor' : undefined"
            :label="projectNames.get(project.id)"
            @update:selected="$event && (tab = project.id)"
            @close="closeProject(project)"
          >
            <GrowingSpinner
              v-if="!readyProjects.has(project.id)"
              :phase="loadingProjectSpinnerPhase(project)"
              :size="16"
            />
          </SelectableTab>
          <SelectableTab
            v-if="tab === 'settings'"
            layoutId="tab-highlight"
            :selected="true"
            icon="settings"
            label="Settings"
          />
        </div>
        <div class="filler" />
        <UserBar :goToSettingsPage="() => (tab = 'settings')" @signOut="onSignOut" />
      </div>
      <div id="appContainerMainView" class="mainView">
        <div class="panel">
          <KeepAlive>
            <Drive v-if="tab === 'drive'" :initialProjectName="initialProjectName" />
          </KeepAlive>
          <div
            v-for="project in openedProjects"
            :key="project.id"
            class="editor"
            :class="{ hidden: tab !== project.id }"
          >
            <Editor
              :hidden="tab !== project.id"
              :project="project"
              @readyUpdate="setProjectReady(project.id, $event)"
              @nameUpdate="projectNames.set(project.id, $event)"
            />
          </div>

          <KeepAlive>
            <Settings v-if="tab === 'settings'" />
          </KeepAlive>
        </div>
        <RightPanel />
      </div>
    </RightPanelDataProviderForReact>
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
</style>
