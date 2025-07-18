<script lang="ts">
import DashboardReact from '#/pages/dashboard/Dashboard'
import { DashboardProps } from '#/pages/dashboard/Dashboard/types'
import { AssetType, EnsoPath } from '#/services/Backend'
import { useBackends } from '$/providers/backends'
import { provideContainerData } from '$/providers/container'
import { provideOpenedProjects } from '$/providers/openedProjects'
import { ContainerDataProviderForReact } from '$/providers/react/container'
import type { DataLoader } from '$/router/dataLoader'
import { backendQueryOptions } from '@/composables/backend'
import { provideAsyncResources } from '@/providers/asyncResources'
import { Ok } from '@/util/data/result'
import { reactComponent } from '@/util/react'
import { useQueryClient } from '@tanstack/vue-query'
import { isRemoteAssetPath } from 'enso-common/src/services/Backend'

const Dashboard = reactComponent(DashboardReact)

export const dataLoader: DataLoader<DashboardProps> = {
  async beforeRouteEnter(to) {
    if (to.params.path == null) return Ok({})
    const { localBackend, remoteBackend } = useBackends()
    const queryClient = useQueryClient()

    const path = EnsoPath(
      to.params.path instanceof Array ? to.params.path.join('/') : to.params.path,
    )

    if (path == null) return Ok({})
    const backend = isRemoteAssetPath(path) ? remoteBackend : localBackend
    if (backend == null) return Ok({})
    const resolvedPath = await backend.resolveEnsoPath(path).catch(() => null)
    if (resolvedPath == null) return Ok({})
    const asset = await queryClient.fetchQuery(
      backendQueryOptions('getAssetDetails', [resolvedPath.id], backend),
    )
    if (asset?.type === AssetType.project) {
      return Ok({ projectToOpen: { asset: { ...asset, ensoPath: path }, backend: backend.type } })
    } else {
      return Ok({})
    }
  },
}
</script>

<script setup lang="ts">
const props = defineProps<DashboardProps>()

const openedProjectsStore = provideOpenedProjects()
provideAsyncResources(openedProjectsStore)
<<<<<<< HEAD
const { tab, openedProjects } = toRefs(provideContainerData(toRef(props, 'launchedProjects')))
provideRightPanelData(tab, props.isFeatureUnderPaywall)
provideFullscreenRoot(fullscreenRoot)

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

const actionHandlers = registerHandlers({
  'app.closeTab': {
    action: () => {
      switch (tab.value) {
        case 'settings':
          closeSettingsTab()
          break
        case 'drive':
          break
        default: {
          // project id
          const project = openedProjects.value.find((proj) => proj.id === tab.value)
          if (project) props.closeProject(project)
          break
        }
      }
    },
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
  void props.closeAllProjects()
}
</script>
<template>
  <div class="TabView">
    <RightPanelDataProviderForReact>
      <div class="bar">
        <div role="tablist" class="tablist">
          <SelectableTab
            selectionLayoutId="tab-highlight"
            :selected="tab === 'drive'"
            icon="drive"
            label="Data Catalog"
            @update:selected="$event && (tab = 'drive')"
          />
          <SelectableTab
            v-for="project in launchedProjects"
            :key="project.id"
            data-testid="editor-tab-button"
            selectionLayoutId="tab-highlight"
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
            selectionLayoutId="tab-highlight"
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
        <div ref="fullscreenRoot" class="FullscreenRoot" @wheel.stop />
      </div>
    </RightPanelDataProviderForReact>
=======
provideContainerData()
</script>
<template>
  <div class="TabView">
    <ContainerDataProviderForReact>
      <Dashboard v-bind="props" />
    </ContainerDataProviderForReact>
>>>>>>> 8bef879aac6e54c051064977463aaed3be15beca
  </div>
</template>

<style scoped>
.TabView {
  --tab-highlight: var(--color-dashboard-background);
  display: flex;
  flex-direction: column;
  height: 100%;
}
</style>
