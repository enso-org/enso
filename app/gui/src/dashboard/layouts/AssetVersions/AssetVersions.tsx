/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as reactQuery from '@tanstack/react-query'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as textProvider from '#/providers/TextProvider'

import AssetVersion from '#/layouts/AssetVersions/AssetVersion'

import type Backend from '#/services/Backend'
import * as backendService from '#/services/Backend'

import { Result } from '#/components/Result'
import type AssetTreeNode from '#/utilities/AssetTreeNode'
import * as dateTime from '#/utilities/dateTime'
import * as uniqueString from 'enso-common/src/utilities/uniqueString'

// ==============================
// === AddNewVersionVariables ===
// ==============================

/** Variables for the "add new version" mutation. */
interface AddNewVersionVariables {
  readonly versionId: backendService.S3ObjectVersionId
  readonly placeholderId: backendService.S3ObjectVersionId
}

// =====================
// === AssetVersions ===
// =====================

/** Props for a {@link AssetVersions}. */
export interface AssetVersionsProps {
  readonly backend: Backend
  readonly item: AssetTreeNode | null
}

/**
 * Display a list of previous versions of an asset.
 */
export default function AssetVersions(props: AssetVersionsProps) {
  const { item, backend } = props

  const { getText } = textProvider.useText()

  if (backend.type === backendService.BackendType.local) {
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

  return <AssetVersionsInternal {...props} item={item} />
}

/**
 * Props for a {@link AssetVersionsInternal}.
 */
interface AssetVersionsInternalProps extends AssetVersionsProps {
  readonly item: AssetTreeNode
}

/**
 * Internal implementation of {@link AssetVersions}.
 */
function AssetVersionsInternal(props: AssetVersionsInternalProps) {
  const { backend, item } = props

  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  const [placeholderVersions, setPlaceholderVersions] = React.useState<
    readonly backendService.S3ObjectVersion[]
  >([])

  const queryKey = [backend.type, 'listAssetVersions', item.item.id, item.item.title]

  const versionsQuery = reactQuery.useSuspenseQuery({
    queryKey,
    queryFn: () =>
      backend
        .listAssetVersions(item.item.id, item.item.title)
        .then((assetVersions) => assetVersions.versions),
  })

  const latestVersion = versionsQuery.data.find((version) => version.isLatest)

  const restoreMutation = reactQuery.useMutation({
    mutationFn: async (variables: AddNewVersionVariables) => {
      if (item.item.type === backendService.AssetType.project) {
        await backend.restoreProject(item.item.id, variables.versionId, item.item.title)
      }
    },
    onMutate: (variables) => {
      setPlaceholderVersions((oldVersions) => [
        {
          isLatest: false,
          key: uniqueString.uniqueString(),
          lastModified: dateTime.toRfc3339(new Date()),
          versionId: variables.placeholderId,
        },
        ...oldVersions,
      ])
    },
    onSuccess: async () => {
      // `backend.restoreProject` does not return the ID of the new version, so a full refetch is
      // necessary.
      await versionsQuery.refetch()
    },
    onError: (error: unknown) => {
      toastAndLog('restoreProjectError', error, item.item.title)
    },
    onSettled: (_data, _error, variables) => {
      setPlaceholderVersions((oldVersions) =>
        oldVersions.filter((version) => version.versionId !== variables.placeholderId),
      )
    },
  })

  return (
    <div className="pointer-events-auto flex flex-1 shrink-0 flex-col items-center overflow-y-auto overflow-x-hidden">
      {versionsQuery.data.length === 0 ?
        <div>{getText('noVersionsFound')}</div>
      : latestVersion == null ?
        <div>{getText('fetchLatestVersionError')}</div>
      : [
          ...placeholderVersions.map((version, i) => (
            <AssetVersion
              key={version.versionId}
              placeholder
              number={versionsQuery.data.length + placeholderVersions.length - i}
              version={version}
              item={item}
              backend={backend}
              latestVersion={latestVersion}
              doRestore={() => {}}
            />
          )),
          ...versionsQuery.data.map((version, i) => (
            <AssetVersion
              key={version.versionId}
              number={versionsQuery.data.length - i}
              version={version}
              item={item}
              backend={backend}
              latestVersion={latestVersion}
              doRestore={() =>
                restoreMutation.mutateAsync({
                  versionId: version.versionId,
                  placeholderId: backendService.S3ObjectVersionId(uniqueString.uniqueString()),
                })
              }
            />
          )),
        ]
      }
    </div>
  )
}
