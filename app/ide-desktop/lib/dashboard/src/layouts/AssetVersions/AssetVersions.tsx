/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'

import AssetVersion from '#/layouts/AssetVersion'

import Spinner from '#/components/Spinner'
import * as spinnerModule from '#/components/Spinner'

import RemoteBackend from '#/services/RemoteBackend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'

import * as assetVersions from './useAssetVersions'

// =====================
// === AssetVersions ===
// =====================

/** Props for a {@link AssetVersions}. */
export interface AssetVersionsProps {
  readonly item: AssetTreeNode
}

/** A list of previous versions of an asset. */
export default function AssetVersions(props: AssetVersionsProps) {
  const { item } = props

  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  const isRemote = backend instanceof RemoteBackend

  const {
    status,
    error,
    data: versions,
    isPending,
  } = assetVersions.useAssetVersions({
    backend,
    assetId: item.item.id,
    title: item.item.title,
    onError: backendError => toastAndLog('Could not list versions', backendError),
    enabled: isRemote,
  })

  const latestVersion = versions?.find(version => version.isLatest)

  return (
    <div className="flex flex-col overflow-y-auto overflow-x-hidden flex-1 shrink-0 items-center">
      {(() => {
        if (!isRemote) {
          return <div>Local assets do not have versions</div>
        } else if (isPending) {
          return <Spinner size={32} state={spinnerModule.SpinnerState.loadingMedium} />
        } else if (status === 'error') {
          return <div>Error: {error.message}</div>
        } else if (versions.length === 0) {
          return <div>No versions found</div>
        } else if (!latestVersion) {
          return <div>Could not fetch the latest version of the file</div>
        } else {
          return versions.map((version, i) => (
            <AssetVersion
              key={version.versionId}
              number={versions.length - i}
              version={version}
              projectId={item.item.id}
              backend={backend}
              latestVersion={latestVersion}
            />
          ))
        }
      })()}
    </div>
  )
}
