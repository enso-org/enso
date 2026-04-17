/** @file A list of previous versions of an asset. */
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Result } from '#/components/Result'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { CATEGORY_BACKEND } from '$/providers/category'
import { useBackends, useText } from '$/providers/react'
import {
  useContainerData,
  useRightPanelContextCategory,
  useRightPanelFocusedAsset,
} from '$/providers/react/container'
import { includes } from '$/utils/data/array'
import { useMutation, useQueryClient, useSuspenseQuery } from '@tanstack/react-query'
import type {
  AnyAsset,
  AssetVersions as AssetVersionsResponse,
  DatalinkAsset,
  FileAsset,
  ProjectAsset,
} from 'enso-common/src/services/Backend'
import { AssetType, BackendType, S3ObjectVersionId } from 'enso-common/src/services/Backend'
import type { RemoteBackend } from 'enso-common/src/services/RemoteBackend'
import { uniqueString } from 'enso-common/src/utilities/uniqueString'
import { AssetVersion, type DuplicateOptions, type Version } from './AssetVersion'
import { assetVersionsQueryOptions } from './queries'

/** Variables for the "add new version" mutation. */
interface AddNewVersionVariables {
  readonly versionId: S3ObjectVersionId
  readonly placeholderId: S3ObjectVersionId
}

/** Context stored while optimistically updating a version comment. */
interface UpdateCommentMutationContext {
  readonly previousVersions?: AssetVersionsResponse
}

/** Display a list of previous versions of an asset. */
export function AssetVersions() {
  const { remoteBackend } = useBackends()
  const { getText } = useText()
  const focusedAsset = useRightPanelFocusedAsset()
  const category = useRightPanelContextCategory()

  if (category == null || CATEGORY_BACKEND[category.type] !== BackendType.remote) {
    return (
      <Result
        status="info"
        centered
        title={getText('assetVersions.localAssetsDoNotHaveVersions')}
      />
    )
  }

  if (focusedAsset == null) {
    return <Result status="info" centered title={getText('assetVersions.notSelected')} />
  }

  if (!isAllowedAssetType(focusedAsset)) {
    return <Result status="info" centered title={getText('assetVersions.invalidAssetType')} />
  }

  return (
    <ErrorBoundary>
      <AssetVersionsInternal backend={remoteBackend} item={focusedAsset} />
    </ErrorBoundary>
  )
}

/** Props for an {@link AssetVersionsInternal}. */
interface AssetVersionsInternalProps {
  readonly item: DatalinkAsset | FileAsset | ProjectAsset
  readonly backend: RemoteBackend
}

/** Internal implementation of {@link AssetVersions}. */
function AssetVersionsInternal(props: AssetVersionsInternalProps) {
  const { backend, item } = props

  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const queryClient = useQueryClient()

  const queryOptions = assetVersionsQueryOptions({ assetId: item.id, backend })

  const versionsQuery = useSuspenseQuery({
    ...queryOptions,
    select: (data) =>
      data.versions.map((version, index) => {
        const number = data.versions.length - index
        const title = getText('versionX', number)
        const tags = [
          ...(version.isLatest ? [getText('latestIndicator')] : []),
          ...(version.tags ?? []),
        ]

        return { ...version, number, title, tags }
      }),
  })

  const versions = versionsQuery.data
  const latestVersion = versions.find((version) => version.isLatest)

  const { openProjectLocally } = useContainerData()

  const restoreMutation = useMutation({
    mutationFn: (variables: AddNewVersionVariables) =>
      backend.restoreAsset(item.id, variables.versionId),
    onError: (error: unknown, _variables) => {
      toastAndLog('restoreProjectError', error, item.title)
    },
    meta: { invalidates: [queryOptions.queryKey], awaitInvalidates: true },
  })

  const duplicateProjectMutation = useMutation(backendMutationOptions(backend, 'copyAsset'))
  const updateCommentMutation = useMutation(
    backendMutationOptions(backend, 'updateAsset', {
      onMutate: async ([, { versionId, comment }]) => {
        await queryClient.cancelQueries({ queryKey: queryOptions.queryKey })

        const previousVersions = queryClient.getQueryData<AssetVersionsResponse>(
          queryOptions.queryKey,
        )

        if (versionId != null) {
          queryClient.setQueryData<AssetVersionsResponse>(
            queryOptions.queryKey,
            (currentVersions) => {
              if (currentVersions == null) {
                return currentVersions
              }

              return {
                ...currentVersions,
                versions: currentVersions.versions.map((version) =>
                  version.versionId === versionId ?
                    { ...version, comment: comment ?? undefined }
                  : version,
                ),
              }
            },
          )
        }

        return { previousVersions }
      },
      onError: (error, _variables, onMutateResult) => {
        const context = isUpdateCommentMutationContext(onMutateResult) ? onMutateResult : undefined
        if (context?.previousVersions != null) {
          queryClient.setQueryData(queryOptions.queryKey, context.previousVersions)
        }
        toastAndLog('updateAssetBackendError', error, item.title)
      },
    }),
  )

  const doDuplicate = useEventCallback(async (options?: DuplicateOptions) => {
    const newItem = await duplicateProjectMutation.mutateAsync([
      item.id,
      item.parentId,
      options?.versionId,
    ])
    const newAsset = newItem.asset

    if (options?.start === true && newAsset.type === AssetType.project) {
      openProjectLocally(newAsset, backend.type)
    }
  })

  const doRestore = useEventCallback((version: Version) =>
    restoreMutation.mutateAsync({
      versionId: version.versionId,
      placeholderId: S3ObjectVersionId(uniqueString()),
    }),
  )

  const doUpdateComment = useEventCallback(async (version: Version, comment: string | null) => {
    await updateCommentMutation.mutateAsync([
      item.id,
      { versionId: version.versionId, comment },
      item.title,
    ])
  })

  if (versions.length === 0) {
    return <Result status="info" centered title={getText('noVersionsFound')} />
  }

  if (latestVersion == null) {
    return <Result status="error" centered title={getText('fetchLatestVersionError')} />
  }

  return (
    <div className="flex h-full w-full flex-col overflow-auto">
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
            doUpdateComment={doUpdateComment}
            isUpdatingComment={
              updateCommentMutation.isPending &&
              updateCommentMutation.variables[1].versionId === version.versionId
            }
          />
          {index !== versions.length - 1 && <div className="ml-[3px] h-5 w-[0.5px] bg-primary" />}
        </div>
      ))}
    </div>
  )
}

/** Check if the asset is allowed to have versions. */
function isAllowedAssetType(asset: AnyAsset): asset is DatalinkAsset | FileAsset | ProjectAsset {
  return includes([AssetType.project, AssetType.datalink, AssetType.file], asset.type)
}

/** Check whether a mutation context belongs to the version comment optimistic update. */
function isUpdateCommentMutationContext(value: unknown): value is UpdateCommentMutationContext {
  return typeof value === 'object' && value != null
}
