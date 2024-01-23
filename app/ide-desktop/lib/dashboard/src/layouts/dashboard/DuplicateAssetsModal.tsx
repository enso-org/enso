/** @file A modal opened when uploaded assets. */
import * as React from 'react'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/backend'
import * as fileInfo from '#/utilities/fileInfo'
import * as string from '#/utilities/string'

import AssetSummary from '#/components/dashboard/AssetSummary'
import Modal from '#/components/Modal'

// =================
// === Constants ===
// =================

// This is a function, even though it does not look like one.
// eslint-disable-next-line no-restricted-syntax
const pluralizeFile = string.makePluralize('file', 'files')
// This is a function, even though it does not look like one.
// eslint-disable-next-line no-restricted-syntax
const pluralizeProject = string.makePluralize('project', 'projects')
// This is a function, even though it does not look like one.
// eslint-disable-next-line no-restricted-syntax
const pluralizeFileUppercase = string.makePluralize('File', 'Files')
// This is a function, even though it does not look like one.
// eslint-disable-next-line no-restricted-syntax
const pluralizeProjectUppercase = string.makePluralize('Project', 'Projects')

// =============
// === Types ===
// =============

/** An object containing the current asset, and the asset that is about to be uploaded,
 * that will conflict with the existing asset. */
export interface ConflictingAsset<
  Asset extends backendModule.FileAsset | backendModule.ProjectAsset =
    | backendModule.FileAsset
    | backendModule.ProjectAsset,
> {
  readonly current: backendModule.AnyAsset
  readonly new: Asset
  readonly file: File
}

// =================================
// === UpdateOrRenameAssetsModal ===
// =================================

/** Props for a {@link DuplicateAssetsModal}. */
export interface DuplicateAssetsModalProps {
  readonly parentKey: backendModule.DirectoryId
  readonly parentId: backendModule.DirectoryId
  readonly conflictingFiles: readonly ConflictingAsset<backendModule.FileAsset>[]
  readonly conflictingProjects: readonly ConflictingAsset<backendModule.ProjectAsset>[]
  readonly dispatchAssetEvent: (assetEvent: assetEvent.AssetEvent) => void
  readonly dispatchAssetListEvent: (assetListEvent: assetListEvent.AssetListEvent) => void
  readonly siblingFileNames: Iterable<string>
  readonly siblingProjectNames: Iterable<string>
  readonly nonConflictingCount: number
  readonly doUploadNonConflicting: () => void
}

/** A modal for creating a new label. */
export default function DuplicateAssetsModal(props: DuplicateAssetsModalProps) {
  const { parentKey, parentId, conflictingFiles: conflictingFilesRaw } = props
  const { conflictingProjects: conflictingProjectsRaw } = props
  const { dispatchAssetEvent, dispatchAssetListEvent } = props
  const { siblingFileNames: siblingFileNamesRaw } = props
  const { siblingProjectNames: siblingProjectNamesRaw } = props
  const { nonConflictingCount, doUploadNonConflicting } = props
  const { unsetModal } = modalProvider.useSetModal()
  const [conflictingFiles, setConflictingFiles] = React.useState(conflictingFilesRaw)
  const [conflictingProjects, setConflictingProjects] = React.useState(conflictingProjectsRaw)
  const siblingFileNames = React.useRef(new Set<string>())
  const siblingProjectNames = React.useRef(new Set<string>())
  const count = conflictingFiles.length + conflictingProjects.length
  const firstConflict = conflictingFiles[0] ?? conflictingProjects[0]
  let firstConflictTypeName: string
  switch (firstConflict?.new.type) {
    case backendModule.AssetType.file: {
      firstConflictTypeName = 'File'
      break
    }
    case backendModule.AssetType.project: {
      firstConflictTypeName = 'Project'
      break
    }
    // eslint-disable-next-line no-restricted-syntax
    case undefined: {
      // This variable does not matter as it should not be used.
      firstConflictTypeName = 'Unknown Asset'
    }
  }
  const otherFilesCount = Math.max(0, conflictingFiles.length - 1)
  const otherFilesText =
    otherFilesCount === 0 ? '' : `and ${otherFilesCount} other ${pluralizeFile(otherFilesCount)}`
  const otherProjectsCount = conflictingProjects.length - (conflictingFiles.length > 0 ? 0 : 1)
  const otherProjectsText =
    otherProjectsCount === 0
      ? ''
      : `and ${otherProjectsCount}${conflictingFiles.length > 0 ? '' : ' other'} ${pluralizeProject(
          otherProjectsCount
        )}`
  const filesTextUppercase = pluralizeFileUppercase(conflictingFiles.length)
  const projectsTextUppercase = pluralizeProjectUppercase(conflictingProjects.length)

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
          const candidateTitle = `${basename} (${i}).${extension}`
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
          const candidateTitle = `${title} (${i})`
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
      files: new Map(toUpdate.map(asset => [asset.current.id, asset.file])),
    })
  }

  const doRename = (toRename: ConflictingAsset[]) => {
    const clonedConflicts = structuredClone(toRename)
    for (const conflict of clonedConflicts) {
      conflict.new.title = findNewName(conflict)
    }
    dispatchAssetListEvent({
      type: AssetListEventType.insertAssets,
      parentKey,
      parentId,
      assets: clonedConflicts.map(conflict => conflict.new),
    })
    dispatchAssetEvent({
      type: AssetEventType.uploadFiles,
      files: new Map(clonedConflicts.map(conflict => [conflict.new.id, conflict.file])),
    })
  }

  return (
    <Modal centered className="absolute bg-dim">
      <form
        data-testid="new-label-modal"
        tabIndex={-1}
        className="relative flex flex-col gap-2 rounded-2xl pointer-events-auto w-96 p-4 pt-2 before:inset-0 before:absolute before:rounded-2xl before:bg-frame-selected before:backdrop-blur-3xl before:w-full before:h-full"
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
          Duplicate{' '}
          {conflictingFiles.length > 0
            ? conflictingProjects.length > 0
              ? `${filesTextUppercase} and ${projectsTextUppercase}`
              : filesTextUppercase
            : projectsTextUppercase}{' '}
          Found
        </h1>
        {nonConflictingCount > 0 && (
          <div className="relative flex flex-col gap-0.5">
            <span>
              {nonConflictingCount} {pluralizeFile(nonConflictingCount)} without conflicts
            </span>
            <button
              type="button"
              className="relative self-start hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
              onClick={doUploadNonConflicting}
            >
              Upload
            </button>
          </div>
        )}
        {firstConflict && (
          <>
            <div className="flex flex-col">
              <span className="relative">Current:</span>
              <AssetSummary asset={firstConflict.current} className="relative" />
            </div>
            <div className="flex flex-col">
              <span className="relative">New:</span>
              <AssetSummary
                new
                newName={backendModule.stripProjectExtension(findNewName(firstConflict, false))}
                asset={firstConflict.new}
                className="relative"
              />
            </div>
            {count > 1 && (
              <div className="relative flex gap-2">
                <button
                  type="button"
                  className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
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
                  Update
                </button>
                <button
                  type="button"
                  className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
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
                  Rename New {firstConflictTypeName}
                </button>
              </div>
            )}
          </>
        )}
        {(otherFilesText !== '' || otherProjectsText !== '' || nonConflictingCount > 0) && (
          <span className="relative">{[otherFilesText, otherProjectsText].join(' ')}</span>
        )}
        <div className="relative flex gap-2">
          <button
            type="button"
            className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
            onClick={() => {
              unsetModal()
              doUploadNonConflicting()
              doUpdate([...conflictingFiles, ...conflictingProjects])
            }}
          >
            {count === 1 ? 'Update' : 'Update All'}
          </button>
          <button
            type="button"
            className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
            onClick={() => {
              unsetModal()
              doUploadNonConflicting()
              doRename([...conflictingFiles, ...conflictingProjects])
            }}
          >
            {count === 1
              ? `Rename New ${firstConflictTypeName}`
              : `Rename New ${firstConflictTypeName}s`}
          </button>
          <button
            type="button"
            className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1"
            onClick={unsetModal}
          >
            Cancel
          </button>
        </div>
      </form>
    </Modal>
  )
}
