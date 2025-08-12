<script lang="ts">
import { Dashboard as DashboardReact, type DashboardProps } from '#/pages/dashboard/Dashboard'
import { EnsoPath } from '#/services/Backend'
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
import {
  AssetDetailsResponse,
  AssetType,
  extractTypeFromId,
  isRemoteAssetPath,
  ProjectAsset,
  ProjectId,
} from 'enso-common/src/services/Backend'
import { ref } from 'vue'
import { onBeforeRouteUpdate } from 'vue-router'

const Dashboard = reactComponent(DashboardReact)

async function getPropsFromPath(pathParam: string | readonly string[] | undefined) {
  if (pathParam == null) return {}
  const { localBackend, remoteBackend } = useBackends()
  const queryClient = useQueryClient()

  console.log('!', pathParam)
  const path = EnsoPath(pathParam instanceof Array ? pathParam.join('/') : pathParam)

  if (!path) return {}
  const backend = isRemoteAssetPath(path) ? remoteBackend : localBackend
  if (backend == null) return {}
  const resolvedPath = await backend.resolveEnsoPath(path).catch(() => null)
  const typedAsset = resolvedPath && extractTypeFromId(resolvedPath.id)
  if (typedAsset?.type !== AssetType.project) return {}
  const options = backendQueryOptions('getAssetDetails', [typedAsset.id, undefined], backend)
  const assetResponse: AssetDetailsResponse<ProjectId> = await queryClient.fetchQuery(options)
  if (!assetResponse) return {}
  const asset: ProjectAsset = { ...assetResponse, ensoPath: path }
  return { projectToOpen: { asset, backend: backend.type } }
}

export const dataLoader: DataLoader<DashboardProps> = {
  async beforeRouteEnter(to) {
    return Ok(await getPropsFromPath(to.params.path))
  },
}
</script>

<script setup lang="ts">
const props = defineProps<DashboardProps>()

const dashboardProps = ref(props)

onBeforeRouteUpdate(async (to) => {
  console.log('HELLO???')
  const { projectToOpen } = await getPropsFromPath(to.params.path)
  console.log(':U hello?', projectToOpen)
  dashboardProps.value = { projectToOpen }
})

const openedProjectsStore = provideOpenedProjects()
provideAsyncResources(openedProjectsStore)
provideContainerData()
</script>
<template>
  <div class="TabView">
    <ContainerDataProviderForReact>
      <Dashboard v-bind="dashboardProps" />
    </ContainerDataProviderForReact>
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
