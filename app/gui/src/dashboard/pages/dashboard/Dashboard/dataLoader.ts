/* eslint-disable react-hooks/rules-of-hooks */
/** @file {@linke dataLoader} definition for Dashboard page. */
import { AssetType, EnsoPath, type RealAssetId } from '#/services/Backend'
import { useBackends } from '$/providers/backends'
import type { DataLoader } from '$/router/dataLoader'
import { backendQueryOptions } from '@/composables/backend'
import { Ok } from '@/util/data/result'
import { useQueryClient } from '@tanstack/vue-query'
import type { DashboardProps } from './types'

export const dataLoader: DataLoader<DashboardProps> = {
  /** Load information about project to open before navigating to Dashboard page. */
  async beforeRouteEnter(to) {
    const { localBackend, remoteBackend } = useBackends()
    const queryClient = useQueryClient()

    const [type, path] =
      to.params.path instanceof Array ?
        [to.params.path[0], to.params.path.slice(1).join('/')]
      : [to.params.path, '']

    const ensoPath: EnsoPath | null =
      type === 'cloud' ? EnsoPath(`enso://${path}`)
      : type === 'local' ? EnsoPath(path)
      : null
    if (ensoPath == null) return Ok({})
    console.debug('ensoPath', ensoPath)
    const backend = type === 'cloud' ? remoteBackend : localBackend
    if (backend == null) return Ok({})
    console.debug('backend', backend)
    const resolvedPath = await backend.resolveEnsoPath(ensoPath).catch(() => null)
    if (resolvedPath == null) return Ok({})
    console.debug('resolvedPath', resolvedPath)
    const asset = await queryClient.fetchQuery(
      backendQueryOptions(
        'getAssetDetails',
        // eslint-disable-next-line no-restricted-syntax
        [resolvedPath.id as RealAssetId],
        backend,
      ),
    )
    console.debug('asset', asset)
    if (asset?.type === AssetType.project) {
      return Ok({ projectToOpen: { asset: { ...asset, ensoPath }, backend: backend.type } })
    } else {
      return Ok({})
    }
  },
}
