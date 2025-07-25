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
    const typedAsset = resolvedPath && extractTypeFromId(resolvedPath.id)
    if (typedAsset?.type !== AssetType.project) return Ok({})

    const options = backendQueryOptions('getAssetDetails', [typedAsset.id], backend)
    const assetResponse: AssetDetailsResponse<ProjectId> = await queryClient.fetchQuery(options)
    if (!assetResponse) return Ok({})

    const asset: ProjectAsset = {
      ...assetResponse,
      ensoPath: path,
    }
    return Ok({ projectToOpen: { asset, backend: backend.type } })
  },
}
</script>

<script setup lang="ts">
const props = defineProps<DashboardProps>()

const openedProjectsStore = provideOpenedProjects()
provideAsyncResources(openedProjectsStore)
provideContainerData()
</script>
<template>
  <div class="TabView">
    <ContainerDataProviderForReact>
      <Dashboard v-bind="props" />
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
