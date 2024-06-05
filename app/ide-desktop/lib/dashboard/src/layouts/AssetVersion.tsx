/** @file Displays information describing a specific version of an asset. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import CompareIcon from 'enso-assets/compare.svg'
import DuplicateIcon from 'enso-assets/duplicate.svg'
import RestoreIcon from 'enso-assets/restore.svg'

import * as backendHooks from '#/hooks/backendHooks'

import * as textProvider from '#/providers/TextProvider'

import * as assetDiffView from '#/layouts/AssetDiffView'

import * as ariaComponents from '#/components/AriaComponents'
import ButtonRow from '#/components/styled/ButtonRow'

import type Backend from '#/services/Backend'
import * as backendService from '#/services/Backend'

import * as dateTime from '#/utilities/dateTime'

// ====================
// === AssetVersion ===
// ====================

/** Props for a {@link AssetVersion}. */
export interface AssetVersionProps {
  readonly placeholder?: boolean
  readonly item: backendService.AnyAsset
  readonly number: number
  readonly version: backendService.S3ObjectVersion
  readonly latestVersion: backendService.S3ObjectVersion
  readonly backend: Backend
  readonly doRestore: () => Promise<void> | void
}

/** Displays information describing a specific version of an asset. */
export default function AssetVersion(props: AssetVersionProps) {
  const { placeholder = false, number, version, item, backend, latestVersion, doRestore } = props
  const { getText } = textProvider.useText()
  const isProject = item.type === backendService.AssetType.project

  const duplicateProjectMutation = backendHooks.useBackendMutation(backend, 'duplicateProject')

  const doDuplicate = () => {
    if (isProject) {
      duplicateProjectMutation.mutate([item.id, version.versionId])
    }
  }

  return (
    <div
      className={tailwindMerge.twMerge(
        'flex w-full flex-shrink-0 basis-0 select-none flex-row gap-4 rounded-2xl p-2',
        placeholder && 'opacity-50'
      )}
    >
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
                icon={CompareIcon}
                isDisabled={version.isLatest || placeholder}
              />
              <ariaComponents.Tooltip>{getText('compareWithLatest')}</ariaComponents.Tooltip>
            </ariaComponents.TooltipTrigger>
            <ariaComponents.Dialog
              type="fullscreen"
              title={getText('compareVersionXWithLatest', number)}
            >
              {opts => (
                <div className="flex h-full flex-col gap-3">
                  <ButtonRow>
                    <ariaComponents.TooltipTrigger>
                      <ariaComponents.Button
                        variant="icon"
                        aria-label={getText('restoreThisVersion')}
                        icon={RestoreIcon}
                        isDisabled={version.isLatest || placeholder}
                        onPress={async () => {
                          await doRestore()
                          opts.close()
                        }}
                      />
                      <ariaComponents.Tooltip>
                        {getText('restoreThisVersion')}
                      </ariaComponents.Tooltip>
                    </ariaComponents.TooltipTrigger>
                    <ariaComponents.TooltipTrigger>
                      <ariaComponents.Button
                        variant="icon"
                        aria-label={getText('duplicateThisVersion')}
                        icon={DuplicateIcon}
                        isDisabled={placeholder}
                        onPress={() => {
                          doDuplicate()
                          opts.close()
                        }}
                      />
                      <ariaComponents.Tooltip>
                        {getText('duplicateThisVersion')}
                      </ariaComponents.Tooltip>
                    </ariaComponents.TooltipTrigger>
                  </ButtonRow>
                  <assetDiffView.AssetDiffView
                    latestVersionId={latestVersion.versionId}
                    versionId={version.versionId}
                    project={item}
                    backend={backend}
                  />
                </div>
              )}
            </ariaComponents.Dialog>
          </ariaComponents.DialogTrigger>
        )}
        {isProject && (
          <ariaComponents.TooltipTrigger>
            <ariaComponents.Button
              variant="icon"
              aria-label={getText('restoreThisVersion')}
              icon={RestoreIcon}
              isDisabled={version.isLatest || placeholder}
              onPress={doRestore}
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
              isDisabled={placeholder}
              onPress={doDuplicate}
            />
            <ariaComponents.Tooltip>{getText('duplicateThisVersion')}</ariaComponents.Tooltip>
          </ariaComponents.TooltipTrigger>
        )}
      </div>
    </div>
  )
}
