<script lang="ts">
import UserBarReact from '#/layouts/UserBar'
import { LaunchedProject, LaunchedProjectId, TabType } from '#/providers/ProjectsProvider'
import { BackendType, ProjectId } from '#/services/Backend'
import { Drive, Editor, Settings } from '$/components/TabView/reactTabs'
import SelectableTab from '$/components/TabView/SelectableTab.vue'
import GrowingSpinner from '@/components/shared/GrowingSpinner.vue'
import { applyPureReactInVue } from 'veaury'
import { reactive, watch } from 'vue'

const UserBar = applyPureReactInVue(UserBarReact)
</script>

<script setup lang="ts">
const {
  initialProjectName,
  page,
  setPage,
  launchedProjects,
  closeProject,
  closeAllProjects,
  clearLaunchedProjects,
} = defineProps<{
  initialProjectName: string | null
  page: LaunchedProjectId | TabType | null
  setPage(page: LaunchedProjectId | TabType): void
  launchedProjects: readonly LaunchedProject[]
  closeProject(project: LaunchedProject): void
  closeAllProjects(): void
  clearLaunchedProjects(): void
}>()

const readyProjects = reactive(new Set<ProjectId>())
const projectNames = reactive(new Map<ProjectId, string>())

function setProjectReady(project: ProjectId, ready: boolean) {
  if (ready) {
    readyProjects.add(project)
    setPage(project)
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
  () => launchedProjects,
  () => {
    const openedProjects = new Set(launchedProjects.map((proj) => proj.id))
    for (const proj of readyProjects) {
      if (!openedProjects.has(proj)) {
        readyProjects.delete(proj)
      }
    }
    for (const proj of projectNames.keys()) {
      if (!openedProjects.has(proj)) {
        projectNames.delete(proj)
      }
    }
  },
)

const onSignOut = () => {
  setPage('drive')
  void closeAllProjects()
  clearLaunchedProjects()
}
</script>
<template>
  <div class="TabView">
    <div class="bar">
      <div role="tablist" class="tablist">
        <SelectableTab
          :selected="page === 'drive'"
          icon="drive"
          label="Data Catalog"
          @update:selected="$event && setPage('drive')"
        />
        <SelectableTab
          v-for="project in launchedProjects"
          :key="project.id"
          data-testid="editor-tab-button"
          :selected="page === project.id"
          :icon="readyProjects.has(project.id) ? 'graph_editor' : undefined"
          :label="projectNames.get(project.id)"
          @update:selected="$event && setPage(project.id)"
          @close="closeProject(project)"
        >
          <GrowingSpinner
            v-if="!readyProjects.has(project.id)"
            :phase="loadingProjectSpinnerPhase(project)"
            :size="16"
          />
        </SelectableTab>
        <SelectableTab
          v-if="page === 'settings'"
          :selected="true"
          icon="settings"
          label="Settings"
        />
      </div>
      <div class="filler" />
      <UserBar :goToSettingsPage="() => setPage('settings')" @signOut="onSignOut" />
    </div>
    <div role="tabpanel" class="panel">
      <KeepAlive>
        <Drive v-if="page === 'drive'" :initialProjectName="initialProjectName" />
      </KeepAlive>
      <!-- instead of v-if we set element hidden, because Editor.tsx is responsible for loading 
       process -->
      <div
        v-for="project in launchedProjects"
        :key="project.id"
        class="editor"
        :class="{ hidden: page !== project.id }"
      >
        <Editor
          :hidden="page !== project.id"
          :project="project"
          @readyUpdate="setProjectReady(project.id, $event)"
          @nameUpdate="projectNames.set(project.id, $event)"
        />
      </div>
      <KeepAlive>
        <Settings v-if="page === 'settings'" />
      </KeepAlive>
    </div>
  </div>
</template>

<style scoped>
.TabView {
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

.panel {
  flex-grow: 1;
  min-height: 0;
  display: flex;
}

.editor {
  display: contents;

  &.hidden {
    display: none;
  }
}
</style>
