<script lang="ts">
import UserBarReact from '#/layouts/UserBar'
import { LaunchedProject, LaunchedProjectId, TabType } from '#/providers/ProjectsProvider'
import { ProjectId } from '#/services/Backend'
import { Drive, Editor, Settings } from '$/components/TabView/reactTabs'
import SelectableTab from '$/components/TabView/SelectableTab.vue'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import SvgIcon from '@/components/SvgIcon.vue'
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
  setIsChatOpen,
} = defineProps<{
  initialProjectName: string | null
  page: LaunchedProjectId | TabType | null
  setPage(page: LaunchedProjectId | TabType): void
  launchedProjects: readonly LaunchedProject[]
  closeProject(project: LaunchedProject): void
  closeAllProjects(): void
  clearLaunchedProjects(): void
  setIsChatOpen(value: boolean): void
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
        <SelectableTab :selected="page === 'drive'" @update:selected="$event && setPage('drive')">
          <SvgIcon name="drive" /><span>Data Catalog</span>
        </SelectableTab>
        <SelectableTab
          v-for="project in launchedProjects"
          :key="project.id"
          data-testid="editor-tab-button"
          :selected="page === project.id"
          @update:selected="$event && setPage(project.id)"
        >
          <SvgIcon v-if="readyProjects.has(project.id)" name="graph_editor" />
          <LoadingSpinner v-else :size="16" />
          <span>{{ projectNames.get(project.id) }}</span>
          <SvgIcon name="close" @click="closeProject(project)" />
        </SelectableTab>
        <SelectableTab v-if="page === 'settings'" :selected="true">Settings</SelectableTab>
      </div>
      <div class="filler" />
      <UserBar
        :goToSettingsPage="() => setPage('settings')"
        :setIsHelpChatOpen="setIsChatOpen"
        @signOut="onSignOut"
      />
    </div>
    <div role="tabpanel" class="panel">
      <KeepAlive>
        <Drive v-if="page === 'drive'" :initialProjectName="initialProjectName" />
      </KeepAlive>
      <Editor
        v-for="project in launchedProjects"
        :key="project.id"
        :hidden="page !== project.id"
        :project="project"
        @readyUpdate="setProjectReady(project.id, $event)"
        @nameUpdate="projectNames.set(project.id, $event)"
      />
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
}

.filler {
  flex-grow: 1;
}

.panel {
  flex-grow: 1;
  min-height: 0;
  display: flex;
}
</style>
