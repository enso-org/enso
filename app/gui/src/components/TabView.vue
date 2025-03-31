<script lang="ts">
import { Suspense } from '#/components/Suspense'

const LazyDrive = lazyReactInVue(() => import('#/layouts/Drive'))
const LazyEditor = lazyReactInVue(() => import('#/layouts/Editor'))
const LazySettings = lazyReactInVue(() => import('#/layouts/Settings'))
</script>

<script setup lang="ts">
import { LaunchedProject, LaunchedProjectId, TabType } from '#/providers/ProjectsProvider'
import { assert } from '@/util/assert'
import { applyPureReactInVue, lazyReactInVue } from 'veaury'
import { computed, h, onMounted, onUnmounted } from 'vue'

const ReactSuspense = applyPureReactInVue(Suspense)

const { initialProjectName, ydocUrl, page, setPage, launchedProjects } = defineProps<{
  initialProjectName: string | null
  ydocUrl: string | null
  page: LaunchedProjectId | TabType | null
  setPage(page: LaunchedProjectId | TabType): void
  launchedProjects: LaunchedProject[]
}>()

// const Drive = applyPureReactInVue(ReactDrive)
// const Editor = applyPureReactInVue(ReactEditor)
// const Settings = applyPureReactInVue(ReactSettings)

// const {
//   page,
//   setPage,
//   launchedProjects,
// }: {
//   page: LaunchedProjectId | TabType | null
//   setPage(page: LaunchedProjectId | TabType): void
//   launchedProjects: LaunchedProject[]
// } = useProjectVueContext()

console.log(launchedProjects, page, initialProjectName, ydocUrl)
const currentComponent = computed(() => {
  switch (page) {
    case null:
    case 'drive':
      return h(LazyDrive, { initialProjectName })
    case 'settings':
      return LazySettings

    default: {
      const project = launchedProjects.find((p) => p.id === page)
      assert(project != null)
      return h(LazyEditor, { project, ydocUrl })
    }
  }
})

onMounted(() => console.error('MOUNTED'))
onUnmounted(() => console.error('UNMOUNTED'))
</script>
<template>
  <div class="TabView">
    <div class="bar">
      <div class="tab" @click="setPage('drive')">Drive</div>
      <div v-for="project in launchedProjects" :key="project.id" @click="setPage(project.id)">
        {{ project.title }}
      </div>
    </div>
  </div>
  <KeepAlive>
    <ReactSuspense>
      <component :is="currentComponent" class="panel" />
    </ReactSuspense>
  </KeepAlive>
</template>
