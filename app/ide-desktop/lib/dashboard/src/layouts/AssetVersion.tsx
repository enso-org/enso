/** @file Displays information describing a specific version of an asset. */
import Duplicate from 'enso-assets/duplicate.svg'

import * as ariaComponents from '#/components/AriaComponents'
import * as dialog from '#/components/AriaComponents/Dialog/Dialog'

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
  const versionName = `version ${number}`

  return (
    <div className="flex flex-row w-full flex-shrink-0 basis-0 gap-4 rounded-2xl p-2 select-none">
      <div className="flex flex-col flex-1">
        <div>{versionName}</div>

        <time className="text-xs text-not-selected">
          on {dateTime.formatDateTime(new Date(version.lastModified))}
        </time>
      </div>

      <div className="flex gap-1 items-center">
        {isProject && (
          <dialog.DialogTrigger>
            <ariaComponents.TooltipTrigger>
              <ariaComponents.Button
                variant="icon"
                aria-label="Compare with HEAD"
                icon={Duplicate}
                isDisabled={version.isLatest}
              />

              <ariaComponents.Tooltip>Compare with HEAD</ariaComponents.Tooltip>
            </ariaComponents.TooltipTrigger>

            <dialog.Dialog type="fullscreen" title={`Compare ${versionName} with HEAD`}>
              <assetDiffView.AssetDiffView
                latestVersionId={latestVersion.versionId}
                versionId={version.versionId}
                projectId={item.id}
                backend={backend}
              />
            </dialog.Dialog>
          </dialog.DialogTrigger>
        )}
      </div>
    </div>
  )
}
