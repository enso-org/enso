<script lang="ts">
import { createGetProjectDetailsQuery, OPENED_PROJECT_STATES } from '#/hooks/projectHooks'
// import DriveReact from '#/layouts/Drive'
// import EditorReact from '#/layouts/Editor'
// import SettingsReact from '#/layouts/Settings'
import UserBarReact from '#/layouts/UserBar'
import { LaunchedProject, LaunchedProjectId, TabType } from '#/providers/ProjectsProvider'
import { BackendType } from '#/services/Backend'
import SvgIcon from '@/components/SvgIcon.vue'
import { useQuery } from '@tanstack/vue-query'
import { applyPureReactInVue } from 'veaury'
import { computed, onMounted, onUnmounted, watch } from 'vue'
import { injectBackendInVue } from './KeepAliveRouterView.vue'
import { Drive, Editor, Settings } from './TabView/reactTabs'

// const LazyDrive = lazyReactInVue(() => import('#/layouts/Drive'))
// const LazyEditor = lazyReactInVue(() => import('#/layouts/Editor'))
// const LazySettings = lazyReactInVue(() => import('#/layouts/Settings'))
// const Drive = applyPureReactInVue(DriveReact)
// const Editor = applyPureReactInVue(EditorReact)
// const Settings = applyPureReactInVue(SettingsReact)
const UserBar = applyPureReactInVue(UserBarReact)
// const ReactSuspenseInVue = applyPureReactInVue(ReactSuspense)
</script>

<script setup lang="ts">
const {
  initialProjectName,
  page,
  setPage,
  launchedProjects,
  closeAllProjects,
  clearLaunchedProjects,
  setIsChatOpen,
} = defineProps<{
  initialProjectName: string | null
  page: LaunchedProjectId | TabType | null
  setPage(page: LaunchedProjectId | TabType): void
  launchedProjects: LaunchedProject[]
  closeAllProjects(): void
  clearLaunchedProjects(): void
  setIsChatOpen(value: boolean): void
}>()

const backend = injectBackendInVue()

const lastProject = computed(() => launchedProjects[launchedProjects.length - 1])
const lastProjectDetailsOptions = computed(() =>
  lastProject.value ?
    createGetProjectDetailsQuery({
      assetId: lastProject.value.id,
      backend:
        lastProject.value.type === BackendType.local ? backend.localBackend : backend.remoteBackend,
    })
  : { queryKey: [] },
)
const lastProjectDetails = useQuery(lastProjectDetailsOptions as any)

watch(
  () => OPENED_PROJECT_STATES.has(lastProjectDetails.data.value?.state.type),
  (isOpened, wasOpened) => {
    if (isOpened === true && wasOpened === false && lastProject.value != null) {
      setPage(lastProject.value.id)
    }
  },
)

const onSignOut = () => {
  setPage('drive')
  closeAllProjects()
  clearLaunchedProjects()
}

// const currentComponent = computed(() => {
//   switch (page) {
//     case null:
//     case 'drive':
//       return [Drive, null] //h(ReactSuspenseInVue, [h(LazyDrive, { initialProjectName })])
//     case 'settings':
//       return [Settings, null]
//     default: {
//       const project = launchedProjects.find((p) => p.id === page)
//       assert(project != null)
//       return [Editor, project]
//     }
//   }
// })
onMounted(() => console.error('TabView MOUNT'))
onUnmounted(() => console.error('TabView UNMOUNT'))
</script>
<template>
  <div class="TabView">
    <div class="bar">
      <div class="tab" @click="setPage('drive')"><SvgIcon name="drive" /><span>Drive</span></div>
      <div
        v-for="project in launchedProjects"
        :key="project.id"
        class="tab"
        @click="setPage(project.id)"
      >
        <SvgIcon name="graph_editor" />
        <span>{{ project.title }}</span>
        <SvgIcon name="close" />
      </div>
      <div v-if="page === 'settings'" class="tab">Settings</div>
      <div class="filler" />
      <UserBar
        :goToSettingsPage="() => setPage('settings')"
        :setIsHelpChatOpen="setIsChatOpen"
        @signOut="onSignOut"
      />
    </div>
  </div>

  <!-- TODO: make it better -->
  <KeepAlive>
    <Drive v-if="page === 'drive'" :initialProjectName="initialProjectName" class="panel" />
  </KeepAlive>
  <KeepAlive v-for="project in launchedProjects">
    <Editor v-if="page === project.id" :project="project" class="panel" />
  </KeepAlive>
  <KeepAlive>
    <Settings v-if="page === 'settings'" class="panel" />
  </KeepAlive>
</template>

<style scoped>
.bar {
  background-color: rgba(0, 0, 0, 0.1);
  display: flex;
  flex-direction: row;
  align-items: center;
  height: 3rem;
}

.tab {
  padding: 0 16px;
  height: 100%;
  display: flex;
  flex-direction: row;
  align-items: center;
  gap: 12px;
}

.filler {
  flex-grow: 1;
}
</style>
