/** @file Displays information describing a specific version of an asset. */
import Duplicate from 'enso-assets/duplicate.svg'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

import type Backend from '#/services/Backend'
import * as backendService from '#/services/Backend'

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
  readonly backend: Backend
}

/** Displays information describing a specific version of an asset. */
export default function AssetVersion(props: AssetVersionProps) {
  const { number, version, item, backend, latestVersion } = props
  const { getText } = textProvider.useText()

  const isProject = item.type === backendService.AssetType.project

  return (
    <div className="flex w-full flex-shrink-0 basis-0 select-none flex-row gap-4 rounded-2xl p-2">
      <div className="flex flex-1 flex-col">
        <div>
          {getText('versionX', number)} {version.isLatest && getText('latestIndicator')}
        </div>

        <time className="text-xs text-not-selected">
          {getText('onDateX', dateTime.formatDateTime(new Date(version.lastModified)))}
        </time>
      </div>

      <div className="flex items-center gap-1">
        {isProject && (
          <ariaComponents.DialogTrigger>
            <ariaComponents.TooltipTrigger>
              <ariaComponents.Button
                variant="icon"
                aria-label={getText('compareWithLatest')}
                icon={Duplicate}
                isDisabled={version.isLatest}
              />
              <ariaComponents.Tooltip>{getText('compareWithLatest')}</ariaComponents.Tooltip>
            </ariaComponents.TooltipTrigger>
            <ariaComponents.Dialog
              type="fullscreen"
              title={getText('compareVersionXWithLatest', number)}
            >
              <assetDiffView.AssetDiffView
                latestVersionId={latestVersion.versionId}
                versionId={version.versionId}
                project={item}
                backend={backend}
              />
            </ariaComponents.Dialog>
          </ariaComponents.DialogTrigger>
        )}
      </div>
    </div>
  )
}
