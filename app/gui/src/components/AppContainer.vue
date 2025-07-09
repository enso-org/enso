<script lang="ts">
import DashboardReact from '#/pages/dashboard/Dashboard'
import { DashboardProps } from '#/pages/dashboard/Dashboard/types'
import { AssetType, EnsoPath, type RealAssetId } from '#/services/Backend'
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

const Dashboard = reactComponent(DashboardReact)

export const dataLoader: DataLoader<DashboardProps> = {
  async beforeRouteEnter(to) {
    const { localBackend, remoteBackend } = useBackends()
    const queryClient = useQueryClient()

    const [type, urlPath] =
      to.params.path instanceof Array ?
        [to.params.path[0], to.params.path.slice(1).join('/')]
      : [to.params.path, '']

    const ensoPath: EnsoPath | null =
      type === 'cloud' ? EnsoPath(`enso://${urlPath}`)
      : type === 'local' ? EnsoPath(urlPath)
      : null

    if (ensoPath == null) return Ok({})
    const backend = type === 'cloud' ? remoteBackend : localBackend
    if (backend == null) return Ok({})
    const resolvedPath = await backend.resolveEnsoPath(ensoPath).catch(() => null)
    if (resolvedPath == null) return Ok({})
    const asset = await queryClient.fetchQuery(
      backendQueryOptions('getAssetDetails', [resolvedPath.id as RealAssetId], backend),
    )
    if (asset?.type === AssetType.project) {
      return Ok({ projectToOpen: { asset: { ...asset, ensoPath }, backend: backend.type } })
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
