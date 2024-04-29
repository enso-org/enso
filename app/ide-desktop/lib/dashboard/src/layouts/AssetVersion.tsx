/** @file Displays information describing a specific version of an asset. */
import * as React from 'react'

import CompareIcon from 'enso-assets/compare.svg'
import DuplicateIcon from 'enso-assets/duplicate.svg'
import RestoreIcon from 'enso-assets/restore.svg'

import * as textProvider from '#/providers/TextProvider'

import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import * as ariaComponents from '#/components/AriaComponents'

import type Backend from '#/services/Backend'
import * as backendService from '#/services/Backend'

import AssetTreeNode from '#/utilities/AssetTreeNode'
import * as dateTime from '#/utilities/dateTime'

import * as assetDiffView from './AssetDiffView'

// ====================
// === AssetVersion ===
// ====================

/** Props for a {@link AssetVersion}. */
export interface AssetVersionProps {
  readonly item: AssetTreeNode
  readonly number: number
  readonly version: backendService.S3ObjectVersion
  readonly latestVersion: backendService.S3ObjectVersion
  readonly backend: Backend
  readonly dispatchAssetListEvent: (event: assetListEvent.AssetListEvent) => void
}

/** Displays information describing a specific version of an asset. */
export default function AssetVersion(props: AssetVersionProps) {
  const { number, version, item, backend, latestVersion, dispatchAssetListEvent } = props
  const { getText } = textProvider.useText()
  const asset = item.item
  const isProject = asset.type === backendService.AssetType.project

  return (
    <div className="flex w-full flex-shrink-0 basis-0 select-none flex-row gap-4 rounded-2xl p-2">
      <div className="flex flex-1 flex-col">
        <div>
          {getText('versionX', number)} {version.isLatest && getText('latestIndicator')}
        </div>

        <time className="text-not-selected text-xs">
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
                icon={CompareIcon}
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
                project={asset}
                backend={backend}
              />
            </ariaComponents.Dialog>
          </ariaComponents.DialogTrigger>
        )}
        {isProject && (
          <ariaComponents.TooltipTrigger>
            <ariaComponents.Button
              variant="icon"
              aria-label={getText('restoreThisVersion')}
              icon={RestoreIcon}
              isDisabled={version.isLatest}
              onPress={() => {
                backend.restoreProject(asset.id, version.versionId, asset.title)
              }}
            />
            <ariaComponents.Tooltip>{getText('restoreThisVersion')}</ariaComponents.Tooltip>
          </ariaComponents.TooltipTrigger>
        )}
        {isProject && (
          <ariaComponents.TooltipTrigger>
            <ariaComponents.Button
              variant="icon"
              aria-label={getText('duplicateThisVersion')}
              icon={DuplicateIcon}
              onPress={() => {
                dispatchAssetListEvent({
                  type: AssetListEventType.duplicateProject,
                  parentKey: item.directoryKey,
                  parentId: asset.parentId,
                  originalId: asset.id,
                  versionId: version.versionId,
                })
              }}
            />
            <ariaComponents.Tooltip>{getText('duplicateThisVersion')}</ariaComponents.Tooltip>
          </ariaComponents.TooltipTrigger>
        )}
      </div>
    </div>
  )
}
