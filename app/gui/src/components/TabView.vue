<script lang="ts">
import UserBarReact from '#/layouts/UserBar'
import { LaunchedProject, LaunchedProjectId, TabType } from '#/providers/ProjectsProvider'
import { ProjectId } from '#/services/Backend'
import LoadingSpinner from '@/components/shared/LoadingSpinner.vue'
import SvgIcon from '@/components/SvgIcon.vue'
import { applyPureReactInVue } from 'veaury'
import { onMounted, onUnmounted, reactive, watch } from 'vue'
import { Drive, Editor, Settings } from './TabView/reactTabs'
import SelectableTab from './TabView/SelectableTab.vue'

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

const projectsReadyState = reactive(new Map<ProjectId, boolean>())

// Automatically open tab once just opened project loads.
const knownReadyProjects = new Set<ProjectId>()
watch(projectsReadyState, (now) => {
  let firstReadyProject: ProjectId | undefined
  for (const [key, state] of now.entries()) {
    console.log('Checking', state, knownReadyProjects.has(key), firstReadyProject)
    if (state && !knownReadyProjects.has(key) && !firstReadyProject) {
      firstReadyProject = key
    }
  }
  for (const previouslyReady of knownReadyProjects) {
    if (!projectsReadyState.get(previouslyReady)) {
      knownReadyProjects.delete(previouslyReady)
    }
  }
  if (firstReadyProject) {
    setPage(firstReadyProject)
  }
})

const onSignOut = () => {
  setPage('drive')
  closeAllProjects()
  clearLaunchedProjects()
}

onMounted(() => console.error('TabView MOUNT'))
onUnmounted(() => console.error('TabView UNMOUNT'))
</script>
<template>
  <div class="TabView">
    <div class="bar">
      <SelectableTab :selected="page === 'drive'" @update:selected="$event && setPage('drive')">
        <SvgIcon name="drive" /><span>Data Catalog</span>
      </SelectableTab>
      <SelectableTab
        v-for="project in launchedProjects"
        :key="project.id"
        :selected="page === project.id"
        @update:selected="$event && setPage(project.id)"
      >
        <SvgIcon v-if="projectsReadyState.get(project.id)" name="graph_editor" />
        <LoadingSpinner v-else :size="16" />
        <span>{{ project.title }}</span>
        <SvgIcon name="close" @click="closeProject(project)" />
      </SelectableTab>
      <SelectableTab v-if="page === 'settings'" :selected="true">Settings</SelectableTab>
      <div class="filler" />
      <UserBar
        :goToSettingsPage="() => setPage('settings')"
        :setIsHelpChatOpen="setIsChatOpen"
        @signOut="onSignOut"
      />
    </div>
    <div class="panel">
      <KeepAlive>
        <Drive v-if="page === 'drive'" :initialProjectName="initialProjectName" />
      </KeepAlive>
      <Editor
        v-for="project in launchedProjects"
        :key="project.id"
        :hidden="page !== project.id"
        :project="project"
        @readyUpdate="
          (console.log('READY UPDATE', $event), projectsReadyState.set(project.id, $event))
        "
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

.filler {
  flex-grow: 1;
}

.panel {
  flex-grow: 1;
  min-height: 0;
  display: flex;
}
</style>
