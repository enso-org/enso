<script lang="ts">
import { Suspense } from '#/components/Suspense'
import { createGetProjectDetailsQuery, OPENED_PROJECT_STATES } from '#/hooks/projectHooks'
import { useBackendInVue } from '#/providers/BackendProvider'
import { LaunchedProject, LaunchedProjectId, TabType } from '#/providers/ProjectsProvider'
import { BackendType } from '#/services/Backend'
import { assert } from '@/util/assert'
import { useQuery } from '@tanstack/vue-query'
import { applyPureReactInVue, lazyReactInVue } from 'veaury'
import { computed, h, watch, watchEffect } from 'vue'

const LazyDrive = lazyReactInVue(() => import('#/layouts/Drive'))
const LazyEditor = lazyReactInVue(() => import('#/layouts/Editor'))
const LazySettings = lazyReactInVue(() => import('#/layouts/Settings'))
</script>

<script setup lang="ts">
const ReactSuspense = applyPureReactInVue(Suspense)

const { initialProjectName, ydocUrl, page, setPage, launchedProjects } = defineProps<{
  initialProjectName: string | null
  ydocUrl: string | null
  page: LaunchedProjectId | TabType | null
  setPage(page: LaunchedProjectId | TabType): void
  launchedProjects: LaunchedProject[]
}>()

const backend = useBackendInVue()

const lastProject = computed(() => launchedProjects[launchedProjects.length - 1])
watchEffect(() => console.log('Last project', lastProject.value))
const lastProjectDetailsOptions = computed(() =>
  lastProject.value ?
    createGetProjectDetailsQuery({
      assetId: lastProject.value.id,
      backend: lastProject.value.type === BackendType.local ? backend.local : backend.remote,
    })
  : { queryKey: [] },
)
const lastProjectDetails = useQuery(lastProjectDetailsOptions as any)
watchEffect(() => console.log('Last project details', lastProjectDetails.data.value))

watch(
  () => OPENED_PROJECT_STATES.has(lastProjectDetails.data.value?.state.type),
  (isOpened, wasOpened) => {
    console.log('IS OPENED', isOpened, 'WAS OPENED', wasOpened)
    if (isOpened === true && wasOpened === false && lastProject.value != null) {
      setPage(lastProject.value.id)
    }
  },
)

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
