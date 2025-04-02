<script lang="ts">
import { Suspense as ReactSuspense } from '#/components/Suspense'
import { createGetProjectDetailsQuery, OPENED_PROJECT_STATES } from '#/hooks/projectHooks'
// import DriveReact from '#/layouts/Drive'
// import EditorReact from '#/layouts/Editor'
// import SettingsReact from '#/layouts/Settings'
import UserBarReact from '#/layouts/UserBar'
import { useBackendInVue } from '#/providers/BackendProvider'
import { LaunchedProject, LaunchedProjectId, TabType } from '#/providers/ProjectsProvider'
import { BackendType } from '#/services/Backend'
import SvgIcon from '@/components/SvgIcon.vue'
import { assert } from '@/util/assert'
import { useQuery } from '@tanstack/vue-query'
import { applyPureReactInVue, lazyReactInVue } from 'veaury'
import { computed, h, onMounted, onUnmounted, watch } from 'vue'

const LazyDrive = lazyReactInVue(() => import('#/layouts/Drive'))
const LazyEditor = lazyReactInVue(() => import('#/layouts/Editor'))
const LazySettings = lazyReactInVue(() => import('#/layouts/Settings'))
// const Drive = applyPureReactInVue(DriveReact)
// const Editor = applyPureReactInVue(EditorReact)
// const Settings = applyPureReactInVue(SettingsReact)
const UserBar = applyPureReactInVue(UserBarReact)
const ReactSuspenseInVue = applyPureReactInVue(ReactSuspense)
</script>

<script setup lang="ts">
const {
  initialProjectName,
  ydocUrl,
  page,
  setPage,
  launchedProjects,
  closeAllProjects,
  clearLaunchedProjects,
  setIsChatOpen,
} = defineProps<{
  initialProjectName: string | null
  ydocUrl: string | null
  page: LaunchedProjectId | TabType | null
  setPage(page: LaunchedProjectId | TabType): void
  launchedProjects: LaunchedProject[]
  closeAllProjects(): void
  clearLaunchedProjects(): void
  setIsChatOpen(value: boolean): void
}>()

const backend = useBackendInVue()

const lastProject = computed(() => launchedProjects[launchedProjects.length - 1])
const lastProjectDetailsOptions = computed(() =>
  lastProject.value ?
    createGetProjectDetailsQuery({
      assetId: lastProject.value.id,
      backend: lastProject.value.type === BackendType.local ? backend.local : backend.remote,
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

const currentComponent = computed(() => {
  switch (page) {
    case null:
    case 'drive':
      return h(ReactSuspenseInVue, [h(LazyDrive, { initialProjectName })])
    case 'settings':
      return LazySettings
    default: {
      const project = launchedProjects.find((p) => p.id === page)
      assert(project != null)
      return h(LazyEditor, { project, ydocUrl })
    }
  }
})
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
  <KeepAlive>
    <component :is="currentComponent" class="panel" />
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
