/** @file A list of previous versions of an asset. */
import * as React from 'react'

import { useMutation, useSuspenseQuery } from '@tanstack/react-query'

import * as uniqueString from 'enso-common/src/utilities/uniqueString'

import { Result } from '#/components/Result'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'
import AssetVersion from '#/layouts/AssetVersions/AssetVersion'
import * as textProvider from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import type { AnyAsset } from '#/services/Backend'
import * as backendService from '#/services/Backend'
import * as dateTime from '#/utilities/dateTime'
import { noop } from '#/utilities/functions'
import { useStore } from '#/utilities/zustand'
import { assetPanelStore } from '../AssetPanel/AssetPanelState'
import { assetVersionsQueryOptions } from './useAssetVersions'

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
}

/**
 * Display a list of previous versions of an asset.
 */
export default function AssetVersions(props: AssetVersionsProps) {
  const { backend } = props

  const { item } = useStore(assetPanelStore, (state) => ({ item: state.assetPanelProps.item }), {
    unsafeEnableTransition: true,
  })

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
  readonly item: AnyAsset
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

  const versionsQuery = useSuspenseQuery(assetVersionsQueryOptions({ assetId: item.id, backend }))

  const latestVersion = versionsQuery.data.find((version) => version.isLatest)

  const restoreMutation = useMutation({
    mutationFn: async (variables: AddNewVersionVariables) => {
      if (item.type === backendService.AssetType.project) {
        await backend.restoreProject(item.id, variables.versionId, item.title)
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
      toastAndLog('restoreProjectError', error, item.title)
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
              doRestore={noop}
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
