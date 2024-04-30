/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetListEvent from '#/events/assetListEvent'

import AssetVersion from '#/layouts/AssetVersion'
import * as useAssetVersions from '#/layouts/AssetVersions/useAssetVersions'

import Spinner from '#/components/Spinner'
import * as spinnerModule from '#/components/Spinner'

import * as backendService from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'
import * as dateTime from '#/utilities/dateTime'
import * as uniqueString from '#/utilities/uniqueString'

// ==============================
// === AddNewVersionVariables ===
// ==============================

/** Variables for the "add new version" mutation. */
export interface AddNewVersionVariables {
  readonly versionId: backendService.S3ObjectVersionId
  readonly placeholderId: backendService.S3ObjectVersionId
}

// =====================
// === AssetVersions ===
// =====================

/** Props for a {@link AssetVersions}. */
export interface AssetVersionsProps {
  readonly item: AssetTreeNode
  readonly dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
}

/** A list of previous versions of an asset. */
export default function AssetVersions(props: AssetVersionsProps) {
  const { item, dispatchAssetListEvent } = props
  const { backend } = backendProvider.useBackend()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const queryClient = reactQuery.useQueryClient()
  const isCloud = backend.type === backendService.BackendType.remote
  const queryKey = ['assetVersions', item.item.id, item.item.title]
  const versionsQuery = useAssetVersions.useAssetVersions({
    backend,
    queryKey,
    assetId: item.item.id,
    title: item.item.title,
    onError: backendError => toastAndLog('listVersionsError', backendError),
    enabled: isCloud,
  })
  const latestVersion = versionsQuery.data?.find(version => version.isLatest)

  const restoreMutation = reactQuery.useMutation({
    mutationFn: async (variables: AddNewVersionVariables) => {
      if (item.item.type === backendService.AssetType.project) {
        await backend.restoreProject(item.item.id, variables.versionId, item.item.title)
      }
    },
    onMutate: async variables => {
      if (item.item.type === backendService.AssetType.project) {
        await queryClient.cancelQueries({ queryKey })
        queryClient.setQueryData<backendService.S3ObjectVersion[]>(queryKey, oldVersions => [
          {
            isLatest: true,
            key: uniqueString.uniqueString(),
            lastModified: dateTime.toRfc3339(new Date()),
            versionId: variables.placeholderId,
          } satisfies backendService.S3ObjectVersion,
          ...(oldVersions?.map(version =>
            version.isLatest ? { ...version, isLatest: false } : version
          ) ?? []),
        ])
      }
    },
    onSuccess: () => {
      if (item.item.type === backendService.AssetType.project) {
        // `backend.restoreProject` does not return the ID of the new version, so a full refetch is
        // necessary.
        versionsQuery.refetch()
      }
    },
    onError: (_error, variables) => {
      if (item.item.type === backendService.AssetType.project) {
        queryClient.setQueryData<backendService.S3ObjectVersion[]>(queryKey, oldVersions => {
          const newVersions =
            oldVersions?.filter(version => version.versionId !== variables.placeholderId) ?? []
          const lastVersion = newVersions[newVersions.length - 1]
          return lastVersion != null && !lastVersion.isLatest
            ? [...newVersions.slice(0, -1), { ...lastVersion, isLatest: true }]
            : newVersions
        })
      }
    },
  })

  return (
    <div className="pointer-events-auto flex flex-1 shrink-0 flex-col items-center overflow-y-auto overflow-x-hidden">
      {!isCloud ? (
        <div>{getText('localAssetsDoNotHaveVersions')}</div>
      ) : versionsQuery.isPending ? (
        <Spinner size={32} state={spinnerModule.SpinnerState.loadingMedium} />
      ) : versionsQuery.isError ? (
        <div>{getText('listVersionsError')}</div>
      ) : versionsQuery.data.length === 0 ? (
        <div>{getText('noVersionsFound')}</div>
      ) : latestVersion == null ? (
        <div>{getText('fetchLatestVersionError')}</div>
      ) : (
        versionsQuery.data.map((version, i) => (
          <AssetVersion
            key={version.versionId}
            number={versionsQuery.data.length - i}
            version={version}
            item={item}
            backend={backend}
            latestVersion={latestVersion}
            dispatchAssetListEvent={dispatchAssetListEvent}
            doRestore={() => {
              restoreMutation.mutate({
                versionId: version.versionId,
                placeholderId: backendService.S3ObjectVersionId(uniqueString.uniqueString()),
              })
            }}
          />
        ))
      )}
    </div>
  )
}
