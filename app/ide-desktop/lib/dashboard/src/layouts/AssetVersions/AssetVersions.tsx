/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as textProvider from '#/providers/TextProvider'

import AssetVersion from '#/layouts/AssetVersion'
import * as useAssetVersions from '#/layouts/AssetVersions/useAssetVersions'

import Spinner from '#/components/Spinner'
import * as spinnerModule from '#/components/Spinner'

import * as backendService from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'

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
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const isCloud = backend.type === backendService.BackendType.remote

  const {
    status,
    data: versions,
    isPending,
  } = useAssetVersions.useAssetVersions({
    backend,
    assetId: item.item.id,
    title: item.item.title,
    onError: backendError => toastAndLog('listVersionsError', backendError),
    enabled: isCloud,
  })

  const latestVersion = versions?.find(version => version.isLatest)

  return (
    <div className="flex flex-1 shrink-0 flex-col items-center overflow-y-auto overflow-x-hidden">
      {(() => {
        if (!isCloud) {
          return <div>{getText('localAssetsDoNotHaveVersions')}</div>
        } else if (isPending) {
          return <Spinner size={32} state={spinnerModule.SpinnerState.loadingMedium} />
        } else if (status === 'error') {
          return <div>{getText('listVersionsError')}</div>
        } else if (versions.length === 0) {
          return <div>{getText('noVersionsFound')}</div>
        } else if (!latestVersion) {
          return <div>{getText('fetchLatestVersionError')}</div>
        } else {
          return versions.map((version, i) => (
            <AssetVersion
              key={version.versionId}
              number={versions.length - i}
              version={version}
              item={item.item}
              backend={backend}
              latestVersion={latestVersion}
            />
          ))
        }
      })()}
    </div>
  )
}
