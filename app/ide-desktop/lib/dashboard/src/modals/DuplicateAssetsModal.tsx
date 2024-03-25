/** @file A modal opened when uploaded assets. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'

import AssetSummary from '#/components/dashboard/AssetSummary'
import Modal from '#/components/Modal'

import * as backendModule from '#/services/Backend'

import * as fileInfo from '#/utilities/fileInfo'
import * as object from '#/utilities/object'

// =============
// === Types ===
// =============

/** An object containing the current asset, and the asset that is about to be uploaded,
 * that will conflict with the existing asset. */
export interface ConflictingAsset<
  Asset extends backendModule.SmartFile | backendModule.SmartProject =
    | backendModule.SmartFile
    | backendModule.SmartProject,
> {
  readonly current: backendModule.AnySmartAsset
  readonly new: Asset
  readonly file: File
}

// =================================
// === UpdateOrRenameAssetsModal ===
// =================================

/** Props for a {@link DuplicateAssetsModal}. */
export interface DuplicateAssetsModalProps {
  readonly parentKey: backendModule.DirectoryId
  readonly parent: backendModule.SmartDirectory
  readonly conflictingFiles: readonly ConflictingAsset<backendModule.SmartFile>[]
  readonly conflictingProjects: readonly ConflictingAsset<backendModule.SmartProject>[]
  readonly dispatchAssetEvent: (assetEvent: assetEvent.AssetEvent) => void
  readonly dispatchAssetListEvent: (assetListEvent: assetListEvent.AssetListEvent) => void
  readonly siblingFileNames: Iterable<string>
  readonly siblingProjectNames: Iterable<string>
  readonly nonConflictingFileCount: number
  readonly nonConflictingProjectCount: number
  readonly doUploadNonConflicting: () => void
}

/** A modal for creating a new label. */
export default function DuplicateAssetsModal(props: DuplicateAssetsModalProps) {
  const { parentKey, parent, conflictingFiles: conflictingFilesRaw } = props
  const { conflictingProjects: conflictingProjectsRaw } = props
  const { dispatchAssetEvent, dispatchAssetListEvent } = props
  const { siblingFileNames: siblingFileNamesRaw } = props
  const { siblingProjectNames: siblingProjectNamesRaw } = props
  const { nonConflictingFileCount, nonConflictingProjectCount, doUploadNonConflicting } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const [conflictingFiles, setConflictingFiles] = React.useState(conflictingFilesRaw)
  const [conflictingProjects, setConflictingProjects] = React.useState(conflictingProjectsRaw)
  const [didUploadNonConflicting, setDidUploadNonConflicting] = React.useState(false)
  const siblingFileNames = React.useRef(new Set<string>())
  const siblingProjectNames = React.useRef(new Set<string>())
  const count = conflictingFiles.length + conflictingProjects.length
  const firstConflict = conflictingFiles[0] ?? conflictingProjects[0]
  const otherFilesCount = Math.max(0, conflictingFiles.length - 1)
  const otherProjectsCount = conflictingProjects.length - (conflictingFiles.length > 0 ? 0 : 1)

  React.useEffect(() => {
    for (const name of siblingFileNamesRaw) {
      siblingFileNames.current.add(name)
    }
    for (const name of siblingProjectNamesRaw) {
      siblingProjectNames.current.add(name)
    }
    // Note that because the props are `Iterable`s, they may be different each time
    // even if their contents are identical. However, as this component should never
    // be re-rendered with different props, the dependency list should not matter anyway.
  }, [siblingFileNamesRaw, siblingProjectNamesRaw])

  const findNewName = (conflict: ConflictingAsset, commit = true) => {
    let title = conflict.file.name
    switch (conflict.new.type) {
      case backendModule.AssetType.file: {
        const { basename, extension } = fileInfo.basenameAndExtension(title)
        let i = 1
        while (true) {
          i += 1
          const candidateTitle = `${basename} ${i}.${extension}`
          if (!siblingFileNames.current.has(candidateTitle)) {
            if (commit) {
              siblingFileNames.current.add(candidateTitle)
            }
            title = candidateTitle
            break
          }
        }
        break
      }
      case backendModule.AssetType.project: {
        const { basename, extension } = backendModule.extractProjectExtension(title)
        title = basename
        let i = 1
        while (true) {
          i += 1
          const candidateTitle = `${title} ${i}`
          if (!siblingProjectNames.current.has(candidateTitle)) {
            if (commit) {
              siblingProjectNames.current.add(candidateTitle)
            }
            title = `${candidateTitle}.${extension}`
            break
          }
        }
        break
      }
    }
    return title
  }

  const doUpdate = (toUpdate: ConflictingAsset[]) => {
    dispatchAssetEvent({
      type: AssetEventType.updateFiles,
      files: new Map(toUpdate.map(asset => [asset.current.value.id, asset.file])),
    })
  }

  const doRename = (toRename: ConflictingAsset[]) => {
    const clonedConflicts = structuredClone(toRename)
    for (const conflict of clonedConflicts) {
      // This is SAFE, as it is a shallow mutation of a freshly cloned object.
      object.unsafeMutable(conflict.new.value).title = findNewName(conflict)
    }
    dispatchAssetListEvent({
      type: AssetListEventType.insertAssets,
      parentKey,
      parent,
      assets: clonedConflicts.map(conflict => conflict.new),
    })
  }

  return (
    <Modal centered className="absolute bg-dim">
      <form
        data-testid="new-label-modal"
        tabIndex={-1}
        className="pointer-events-auto relative flex w-duplicate-assets-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onKeyDown={event => {
          if (event.key !== 'Escape') {
            event.stopPropagation()
          }
        }}
        onClick={event => {
          event.stopPropagation()
        }}
        onSubmit={event => {
          event.preventDefault()
        }}
      >
        <h1 className="relative text-sm font-semibold">
          {conflictingFiles.length > 0
            ? conflictingProjects.length > 0
              ? getText('duplicateFilesAndProjectsFound')
              : getText('duplicateFilesFound')
            : getText('duplicateProjectsFound')}
        </h1>
        {nonConflictingFileCount > 0 ||
          (nonConflictingProjectCount > 0 && (
            <div className="relative flex flex-col">
              {nonConflictingFileCount > 0 && (
                <span className="text">
                  {nonConflictingFileCount === 1
                    ? getText('fileWithoutConflicts')
                    : getText('filesWithoutConflicts', nonConflictingFileCount)}
                </span>
              )}
              {nonConflictingProjectCount > 0 && (
                <span className="text">
                  {nonConflictingProjectCount === 1
                    ? getText('projectWithoutConflicts')
                    : getText('projectsWithoutConflicts', nonConflictingFileCount)}
                </span>
              )}
              <button
                disabled={didUploadNonConflicting}
                type="button"
                className="button relative self-start rounded-full bg-selected-frame selectable enabled:active"
                onClick={() => {
                  doUploadNonConflicting()
                  setDidUploadNonConflicting(true)
                }}
              >
                {didUploadNonConflicting ? getText('uploaded') : getText('upload')}
              </button>
            </div>
          ))}
        {firstConflict && (
          <>
            <div className="flex flex-col">
              <span className="relative">{getText('currentColon')}</span>
              <AssetSummary asset={firstConflict.current.value} className="relative" />
            </div>
            <div className="flex flex-col">
              <span className="relative">{getText('newColon')}</span>
              <AssetSummary
                new
                newName={backendModule.stripProjectExtension(findNewName(firstConflict, false))}
                asset={firstConflict.new.value}
                className="relative"
              />
            </div>
            {count > 1 && (
              <div className="relative flex gap-icons">
                <button
                  type="button"
                  className="button bg-selected-frame active"
                  onClick={() => {
                    doUpdate([firstConflict])
                    switch (firstConflict.new.type) {
                      case backendModule.AssetType.file: {
                        setConflictingFiles(oldConflicts => oldConflicts.slice(1))
                        break
                      }
                      case backendModule.AssetType.project: {
                        setConflictingProjects(oldConflicts => oldConflicts.slice(1))
                        break
                      }
                    }
                  }}
                >
                  {getText('update')}
                </button>
                <button
                  type="button"
                  className="button  bg-selected-frame active"
                  onClick={() => {
                    doRename([firstConflict])
                    switch (firstConflict.new.type) {
                      case backendModule.AssetType.file: {
                        setConflictingFiles(oldConflicts => oldConflicts.slice(1))
                        break
                      }
                      case backendModule.AssetType.project: {
                        setConflictingProjects(oldConflicts => oldConflicts.slice(1))
                        break
                      }
                    }
                  }}
                >
                  {firstConflict.new.type === backendModule.AssetType.file
                    ? getText('renameNewFile')
                    : getText('renameNewProject')}
                </button>
              </div>
            )}
          </>
        )}
        {otherFilesCount > 0 && (
          <span className="relative">
            {otherFilesCount === 1
              ? getText('andOtherFile')
              : getText('andOtherFiles', otherFilesCount)}
          </span>
        )}
        {otherProjectsCount > 0 && (
          <span className="relative">
            {otherProjectsCount === 1
              ? getText('andOtherProject')
              : getText('andOtherProjects', otherProjectsCount)}
          </span>
        )}
        <div className="relative flex gap-icons">
          <button
            type="button"
            className="button bg-invite text-white active"
            onClick={() => {
              unsetModal()
              doUploadNonConflicting()
              doUpdate([...conflictingFiles, ...conflictingProjects])
            }}
          >
            {count === 1 ? getText('update') : getText('updateAll')}
          </button>
          <button
            type="button"
            className="button bg-invite text-white active"
            onClick={() => {
              unsetModal()
              doUploadNonConflicting()
              doRename([...conflictingFiles, ...conflictingProjects])
            }}
          >
            {count === 1
              ? firstConflict?.new.type === backendModule.AssetType.file
                ? getText('renameNewFile')
                : getText('renameNewProject')
              : firstConflict?.new.type === backendModule.AssetType.file
                ? getText('renameNewFiles')
                : getText('renameNewProjects')}
          </button>
          <button type="button" className="button bg-selected-frame active" onClick={unsetModal}>
            {getText('cancel')}
          </button>
        </div>
      </form>
    </Modal>
  )
}
