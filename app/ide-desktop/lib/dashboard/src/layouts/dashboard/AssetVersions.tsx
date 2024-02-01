/** @file A list of previous versions of an asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'

import AssetVersion from '#/layouts/dashboard/AssetVersion'

import type * as backend from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'

// =====================
// === AssetVersions ===
// =====================

/** Props for a {@link AssetVersions}. */
export interface AssetVersionsProps {
  hidden: boolean
  item: AssetTreeNode
}

/** A list of previous versions of an asset. */
export default function AssetVersions(props: AssetVersionsProps) {
  const { hidden, item } = props
  const { backend } = backendProvider.useBackend()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [initialized, setInitialized] = React.useState(false)
  const [versions, setVersions] = React.useState<backend.S3ObjectVersion[]>([])

  React.useEffect(() => {
    if (!hidden && !initialized) {
      setInitialized(true)
      void (async () => {
        try {
          const assetVersions = await backend.listAssetVersions(item.item.id, item.item.title)
          setVersions([...assetVersions.versions].reverse())
        } catch (error) {
          setInitialized(false)
          toastAndLog('Could not list versions', error)
        }
      })()
    }
  }, [
    hidden,
    initialized,
    backend,
    item.item.id,
    item.item.title,
    /* should never change */ toastAndLog,
  ])

  return hidden ? (
    <></>
  ) : (
    <div className="flex flex-col">
      {versions.map((version, i) => (
        <AssetVersion key={version.versionId} number={versions.length - i} version={version} />
      ))}
    </div>
  )
}
