/** @file Displays information describing a specific version of an asset. */
import Duplicate from 'enso-assets/duplicate.svg'

import * as ariaComponents from '#/components/AriaComponents'

import * as backendService from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

import * as dateTime from '#/utilities/dateTime'

import * as assetDiffView from './AssetDiffView'

// ====================
// === AssetVersion ===
// ====================

/** Props for a {@link AssetVersion}. */
export interface AssetVersionProps {
  readonly item: backendService.AnyAsset
  readonly number: number
  readonly version: backendService.S3ObjectVersion
  readonly latestVersion: backendService.S3ObjectVersion
  readonly backend: RemoteBackend
}

/** Displays information describing a specific version of an asset. */
export default function AssetVersion(props: AssetVersionProps) {
  const { number, version, item, backend, latestVersion } = props

  const isProject = item.type === backendService.AssetType.project
  const versionName = `Version ${number}`

  return (
    <div className="flex w-full flex-shrink-0 basis-0 select-none flex-row gap-4 rounded-2xl p-2">
      <div className="flex flex-1 flex-col">
        <div>
          {versionName} {version.isLatest && `(Latest)`}
        </div>

        <time className="text-xs text-not-selected">
          on {dateTime.formatDateTime(new Date(version.lastModified))}
        </time>
      </div>

      <div className="flex items-center gap-1">
        {isProject && (
          <ariaComponents.DialogTrigger>
            <ariaComponents.TooltipTrigger>
              <ariaComponents.Button
                variant="icon"
                aria-label="Compare with latest"
                icon={Duplicate}
                isDisabled={version.isLatest}
              />

              <ariaComponents.Tooltip>Compare with latest</ariaComponents.Tooltip>
            </ariaComponents.TooltipTrigger>

            <ariaComponents.Dialog type="fullscreen" title={`Compare ${versionName} with latest`}>
              <assetDiffView.AssetDiffView
                latestVersionId={latestVersion.versionId}
                versionId={version.versionId}
                projectId={item.id}
                backend={backend}
              />
            </ariaComponents.Dialog>
          </ariaComponents.DialogTrigger>
        )}
      </div>
    </div>
  )
}
