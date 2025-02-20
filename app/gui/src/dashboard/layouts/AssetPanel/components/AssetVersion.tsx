/** @file Displays information describing a specific version of an asset. */
import CompareIcon from '#/assets/compare.svg'
import DuplicateIcon from '#/assets/duplicate.svg'
import RestoreIcon from '#/assets/restore.svg'

import * as textProvider from '#/providers/TextProvider'

import * as assetDiffView from '#/layouts/AssetDiffView'

import * as ariaComponents from '#/components/AriaComponents'

import type Backend from '#/services/Backend'
import * as backendService from '#/services/Backend'

import { duplicateProjectMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useOpenProject } from '#/hooks/projectHooks'
import * as tailwindMerge from '#/utilities/tailwindMerge'
import { useMutation, useQueryClient } from '@tanstack/react-query'
import { formatDateTime } from 'enso-common/src/utilities/data/dateTime'

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
export function AssetVersion(props: AssetVersionProps) {
  const { placeholder = false, number, version, item, backend, latestVersion, doRestore } = props
  const { getText } = textProvider.useText()
  const queryClient = useQueryClient()
  const openProject = useOpenProject()
  const duplicateProjectMutation = useMutation(
    duplicateProjectMutationOptions(backend, queryClient, openProject),
  )
  const isProject = item.type === backendService.AssetType.project

  const doDuplicate = useEventCallback(async () => {
    if (isProject) {
      await duplicateProjectMutation.mutateAsync([
        item.id,
        item.title,
        item.parentId,
        version.versionId,
      ])
    }
  })

  return (
    <div
      className={tailwindMerge.twMerge(
        'flex w-full shrink-0 basis-0 select-none flex-row gap-4 rounded-2xl p-2',
        placeholder && 'opacity-50',
      )}
    >
      <div className="flex flex-1 flex-col">
        <div>
          {getText('versionX', number)} {version.isLatest && getText('latestIndicator')}
        </div>

        <time className="text-xs text-not-selected">
          {getText('onDateX', formatDateTime(new Date(version.lastModified)))}
        </time>
      </div>

      <div className="flex items-center gap-3">
        {isProject && (
          <ariaComponents.DialogTrigger>
            <ariaComponents.Button
              size="medium"
              variant="icon"
              aria-label={getText('compareWithLatest')}
              icon={CompareIcon}
              isDisabled={version.isLatest || placeholder}
            />
            <ariaComponents.Dialog
              type="fullscreen"
              title={getText('compareVersionXWithLatest', number)}
              padding="none"
            >
              {(opts) => (
                <div className="flex h-full flex-col">
                  <ariaComponents.ButtonGroup className="px-4 py-4" gap="large">
                    <ariaComponents.Button
                      size="medium"
                      variant="icon"
                      loaderPosition="icon"
                      icon={RestoreIcon}
                      isDisabled={version.isLatest || placeholder}
                      onPress={async () => {
                        await doRestore()
                        opts.close()
                      }}
                    >
                      {getText('restoreThisVersion')}
                    </ariaComponents.Button>
                    <ariaComponents.Button
                      size="medium"
                      variant="icon"
                      loaderPosition="icon"
                      icon={DuplicateIcon}
                      isDisabled={placeholder}
                      onPress={async () => {
                        await doDuplicate()
                        opts.close()
                      }}
                    >
                      {getText('duplicateThisVersion')}
                    </ariaComponents.Button>
                  </ariaComponents.ButtonGroup>
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
              size="medium"
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
              size="medium"
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
