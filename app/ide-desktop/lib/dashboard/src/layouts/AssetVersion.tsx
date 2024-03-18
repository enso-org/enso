/** @file Displays information describing a specific version of an asset. */
import * as React from 'react'

import Duplicate from 'enso-assets/duplicate.svg'

import { Button, Tooltip, TooltipTrigger } from '#/components/AriaComponents'
import { Dialog, DialogTrigger } from '#/components/AriaComponents/Dialog/Dialog'

import type * as backendService from '#/services/Backend'
import type { AnyAsset } from '#/services/Backend'
import type RemoteBackend from '#/services/RemoteBackend'

import * as dateTime from '#/utilities/dateTime'

import * as assetDiffView from './AssetDiffView'

// ====================
// === AssetVersion ===
// ====================

/** Props for a {@link AssetVersion}. */
export interface AssetVersionProps {
  readonly projectId: AnyAsset['id']
  readonly number: number
  readonly version: backendService.S3ObjectVersion
  readonly latestVersion: backendService.S3ObjectVersion
  readonly backend: RemoteBackend
}

/** Displays information describing a specific version of an asset. */
export default function AssetVersion(props: AssetVersionProps) {
  const { number, version, projectId, backend, latestVersion } = props

  const versionName = `version ${number}`

  return (
    <div className="flex flex-row w-full flex-shrink-0 basis-0 gap-4 rounded-2xl p-2 select-none">
      <div className="flex flex-col flex-1">
        <div>{versionName}</div>

        <time className="text-not-selected text-xs">
          on {dateTime.formatDateTime(new Date(version.lastModified))}
        </time>
      </div>

      <div className="flex gap-1 items-center">
        <DialogTrigger>
          <TooltipTrigger>
            <Button
              variant="icon"
              aria-label="Compare with HEAD"
              icon={Duplicate}
              isDisabled={version.isLatest}
            />

            <Tooltip>Compare with HEAD</Tooltip>
          </TooltipTrigger>

          <Dialog type="fullscreen" title={`Compare ${versionName} with HEAD`}>
            <assetDiffView.AssetDiffView
              latestVersionId={latestVersion.versionId}
              versionId={version.versionId}
              projectId={projectId}
              backend={backend}
            />
          </Dialog>
        </DialogTrigger>
      </div>
    </div>
  )
}
