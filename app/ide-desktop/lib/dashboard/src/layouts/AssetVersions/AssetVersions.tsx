/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as textProvider from '#/providers/TextProvider'

import AssetVersion from '#/layouts/AssetVersion'
import * as useAssetVersions from '#/layouts/AssetVersions/useAssetVersions'

import Spinner from '#/components/Spinner'
import * as spinnerModule from '#/components/Spinner'

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
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()

  const {
    status,
    data: versions,
    isPending,
  } = useAssetVersions.useAssetVersions({
    item: item.item,
    onError: backendError => toastAndLog('listVersionsError', backendError),
  })

  const latestVersion = versions?.find(version => version.isLatest)

  return (
    <div className="pointer-events-auto flex flex-1 shrink-0 flex-col items-center overflow-y-auto overflow-x-hidden">
      {(() => {
        if (isPending) {
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
              latestVersion={latestVersion}
            />
          ))
        }
      })()}
    </div>
  )
}
