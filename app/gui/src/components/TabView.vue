<script setup lang="ts">
import ReactDrive from '#/layouts/Drive'
import ReactEditor from '#/layouts/Editor'
import ReactSettings from '#/layouts/Settings'
import { LaunchedProject, LaunchedProjectId, TabType } from '#/providers/ProjectsProvider'
import { assert } from '@/util/assert'
import * as react from 'react'
import { applyPureReactInVue } from 'veaury'
import { computed, h, onMounted, onUnmounted } from 'vue'

const { initialProjectName, ydocUrl, page, setPage, launchedProjects } = defineProps<{
  initialProjectName: string | null
  ydocUrl: string | null
  page: LaunchedProjectId | TabType | null
  setPage(page: LaunchedProjectId | TabType): void
  launchedProjects: LaunchedProject[]
}>()

const Drive = applyPureReactInVue(ReactDrive)
const Editor = applyPureReactInVue(ReactEditor)
const Settings = applyPureReactInVue(ReactSettings)
const LazyDrive = applyPureReactInVue(react.lazy(() => import('#/layouts/Drive')))
const LazyEditor = applyPureReactInVue(react.lazy(() => import('#/layouts/Editor')))
const LazySettings = applyPureReactInVue(react.lazy(() => import('#/layouts/Settings')))

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
      return h(Drive, { initialProjectName })
    case 'settings':
      return Settings

    default:
      const project = launchedProjects.find((p) => p.id === page)
      assert(project != null)
      return h(Editor, { project, ydocUrl })
  }
})

onMounted(() => console.error('MOUNTED'))
onUnmounted(() => console.error('UNMOUNTED'))
</script>
<template>
  <div class="TabView">
    <div class="bar">
      <div class="tab" @click="setPage('drive')">Drive</div>
      <div
        v-for="(project, index) in launchedProjects"
        :key="project.id"
        @click="setPage(project.id)"
      >
        {{ project.title }}
      </div>
    </div>
  </div>
  <KeepAlive>
    <component :is="currentComponent" class="panel" />
  </KeepAlive>
</template>
