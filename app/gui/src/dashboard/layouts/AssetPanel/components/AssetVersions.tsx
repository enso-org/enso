/** @file A list of previous versions of an asset. */

import { useMutation, useSuspenseQuery } from '@tanstack/react-query'

import { uniqueString } from 'enso-common/src/utilities/uniqueString'

import { Result } from '#/components/Result'
import { copyAssetsMutationOptions } from '#/hooks/backendBatchedHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOpenProjectLocally } from '#/hooks/projectHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import type { AnyAsset, DatalinkAsset, FileAsset, ProjectAsset } from '#/services/Backend'
import { AssetType, BackendType, S3ObjectVersionId } from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'
import { useText } from '$/providers/react'
import { includes } from 'enso-common/src/utilities/data/array'
import { useAssetPanelCurrentItem } from '../AssetPanelState'
import { AssetVersion, type DuplicateOptions, type Version } from './AssetVersion'
import { assetVersionsQueryOptions } from './queries'
import type { AssetPanelProps } from './types'

/** Variables for the "add new version" mutation. */
interface AddNewVersionVariables {
  readonly versionId: S3ObjectVersionId
  readonly placeholderId: S3ObjectVersionId
}

/** Props for an {@link AssetVersions}. */
export interface AssetVersionsProps extends AssetPanelProps {}

/** Display a list of previous versions of an asset. */
export function AssetVersions(props: AssetVersionsProps) {
  const { backend } = props
  const { getText } = useText()

  const item = useAssetPanelCurrentItem()

  if (backend.type === BackendType.local) {
    return (
      <Result
        status="info"
        centered
        title={getText('assetVersions.localAssetsDoNotHaveVersions')}
      />
    )
  }

  if (item == null) {
    return <Result status="info" centered title={getText('assetVersions.notSelected')} />
  }

  if (!isAllowedAssetType(item)) {
    return <Result status="info" centered title={getText('assetVersions.invalidAssetType')} />
  }

  // This is SAFE because we know that the backend is a RemoteBackend.
  // eslint-disable-next-line no-restricted-syntax
  return <AssetVersionsInternal {...props} backend={backend as RemoteBackend} item={item} />
}

/** Props for an {@link AssetVersionsInternal}. */
interface AssetVersionsInternalProps extends AssetVersionsProps {
  readonly item: DatalinkAsset | FileAsset | ProjectAsset
  readonly backend: RemoteBackend
}

/** Internal implementation of {@link AssetVersions}. */
function AssetVersionsInternal(props: AssetVersionsInternalProps) {
  const { backend, item } = props

  const { getText } = useText()
  const toastAndLog = useToastAndLog()

  const queryOptions = assetVersionsQueryOptions({ assetId: item.id, backend })

  const versionsQuery = useSuspenseQuery({
    ...queryOptions,
    select: (data) =>
      data.versions.map((version, index) => {
        const number = data.versions.length - index
        const title = getText('versionX', number)

        return { ...version, number, title }
      }),
  })

  const versions = versionsQuery.data
  const latestVersion = versions.find((version) => version.isLatest)

  const openProjectLocally = useOpenProjectLocally()

  const restoreMutation = useMutation({
    mutationFn: (variables: AddNewVersionVariables) =>
      backend.restoreAsset(item.id, variables.versionId),
    onError: (error: unknown, _variables) => {
      toastAndLog('restoreProjectError', error, item.title)
    },
    meta: { invalidates: [queryOptions.queryKey], awaitInvalidates: true },
  })

  const duplicateProjectMutation = useMutation(copyAssetsMutationOptions(backend))

  const doDuplicate = useEventCallback(async (options?: DuplicateOptions) => {
    const newItem = await duplicateProjectMutation.mutateAsync([[item.id], item.parentId])
    const newAsset = newItem[0]?.asset

    if (options?.start === true && newAsset != null && item.type === AssetType.project) {
      // This is SAFE because we know that the the new asset is a Project,
      // because we can't create a duplicate with a different type.
      /* eslint-disable-next-line no-restricted-syntax */
      await openProjectLocally(newAsset as ProjectAsset, backend.type)
    }
  })

  const doRestore = useEventCallback((version: Version) =>
    restoreMutation.mutateAsync({
      versionId: version.versionId,
      placeholderId: S3ObjectVersionId(uniqueString()),
    }),
  )

  if (versions.length === 0) {
    return <Result status="info" centered title={getText('noVersionsFound')} />
  }

  if (latestVersion == null) {
    return <Result status="error" centered title={getText('fetchLatestVersionError')} />
  }

  return (
    <div className="flex w-full flex-col">
      {versions.map((version, index) => (
        <div key={version.versionId}>
          <AssetVersion
            version={version}
            otherVersions={versions}
            item={item}
            backend={backend}
            previousVersion={versions[index + 1]}
            doRestore={doRestore}
            doDuplicate={doDuplicate}
          />

          {index !== versions.length - 1 && <div className="ml-[3px] h-5 w-[0.5px] bg-primary" />}
        </div>
      ))}
    </div>
  )
}

/**
 * Check if the asset is allowed to have versions.
 */
function isAllowedAssetType(asset: AnyAsset): asset is DatalinkAsset | FileAsset | ProjectAsset {
  return includes([AssetType.project, AssetType.datalink, AssetType.file], asset.type)
}
