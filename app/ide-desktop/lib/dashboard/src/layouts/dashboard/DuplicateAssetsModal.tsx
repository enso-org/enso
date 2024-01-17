/** @file A modal opened when uploaded assets. */
import * as React from 'react'

import type * as assetEvent from '#/events/assetEvent'
import AssetEventType from '#/events/AssetEventType'
import type * as assetListEvent from '#/events/assetListEvent'
import AssetListEventType from '#/events/AssetListEventType'
import * as modalProvider from '#/providers/ModalProvider'
import * as backendModule from '#/services/backend'
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
    readonly doUploadNonConflicting: () => void
}

/** A modal for creating a new label. */
export default function DuplicateAssetsModal(props: DuplicateAssetsModalProps) {
    const { parentKey, parentId, conflictingFiles: conflictingFilesRaw } = props
    const { conflictingProjects: conflictingProjectsRaw } = props
    const { dispatchAssetEvent, dispatchAssetListEvent } = props
    const { siblingFileNames: siblingFileNamesRaw } = props
    const { siblingProjectNames: siblingProjectNamesRaw } = props
    const { doUploadNonConflicting: doUploadNonConflictingRaw } = props
    const { unsetModal } = modalProvider.useSetModal()
    const [didUploadNonConflicting, setDidUploadNonConflicting] = React.useState(false)
    const [conflictingFiles, setConflictingFiles] = React.useState(conflictingFilesRaw)
    const [conflictingProjects, setConflictingProjects] = React.useState(conflictingProjectsRaw)
    const siblingFileNames = React.useRef(new Set<string>())
    const siblingProjectNames = React.useRef(new Set<string>())
    const count = conflictingFiles.length + conflictingProjects.length
    const firstConflict = conflictingFiles[0] ?? conflictingProjects[0]
    const otherFilesText =
        conflictingFiles.length === 0
            ? ''
            : `and ${conflictingFiles.length} other ${pluralizeFile(conflictingFiles.length)}`
    const otherProjectsCount = conflictingProjects.length - (conflictingFiles.length > 0 ? 0 : 1)
    const otherProjectsText =
        otherProjectsCount === 0
            ? ''
            : `and ${otherProjectsCount} other ${pluralizeProject(otherProjectsCount)}`

    const doUploadNonConflicting = React.useCallback(() => {
        if (!didUploadNonConflicting) {
            doUploadNonConflictingRaw()
            setDidUploadNonConflicting(true)
        }
    }, [didUploadNonConflicting, doUploadNonConflictingRaw])

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

    const doUpdate = (toUpdate: ConflictingAsset[]) => {
        dispatchAssetEvent({
            type: AssetEventType.updateFiles,
            files: new Map(toUpdate.map(asset => [asset.current.id, asset.file])),
        })
    }

    const doRename = (toRename: ConflictingAsset[]) => {
        const clonedConflicts = structuredClone(toRename)
        for (const conflict of clonedConflicts) {
            let title = conflict.file.name
            switch (conflict.new.type) {
                case backendModule.AssetType.file: {
                    let i = 1
                    while (true) {
                        i += 1
                        const candidateTitle = `${title} (${i})`
                        if (!siblingFileNames.current.has(candidateTitle)) {
                            siblingFileNames.current.add(candidateTitle)
                            title = candidateTitle
                            break
                        }
                    }
                    break
                }
                case backendModule.AssetType.project: {
                    title = backendModule.stripProjectExtension(title)
                    let i = 1
                    while (true) {
                        i += 1
                        const candidateTitle = `${title} (${i})`
                        if (!siblingProjectNames.current.has(candidateTitle)) {
                            siblingProjectNames.current.add(candidateTitle)
                            title = candidateTitle
                            break
                        }
                    }
                    break
                }
            }
            conflict.new.title = title
        }
        dispatchAssetListEvent({
            type: AssetListEventType.insertAssets,
            parentKey,
            parentId,
            assets: clonedConflicts.map(conflict => conflict.new),
        })
        dispatchAssetEvent({
            type: AssetEventType.uploadFiles,
            files: new Map(toRename.map(asset => [asset.current.id, asset.file])),
        })
    }

    return (
        <Modal className="absolute bg-dim">
            <form
                data-testid="new-label-modal"
                tabIndex={-1}
                className="relative flex flex-col gap-2 rounded-2xl pointer-events-auto w-80 p-4 pt-2 before:inset-0 before:absolute before:rounded-2xl before:bg-frame-selected before:backdrop-blur-3xl before:w-full before:h-full"
                onKeyDown={event => {
                    if (event.key !== 'Escape') {
                        event.stopPropagation()
                    }
                }}
                onClick={event => {
                    event.stopPropagation()
                }}
            >
                <h1 className="relative text-sm font-semibold">
                    Duplicate Files and Projects Found
                </h1>
                {firstConflict && (
                    <AssetSummary asset={firstConflict.current} className="relative" />
                )}
                {firstConflict && <AssetSummary asset={firstConflict.new} className="relative" />}
                {firstConflict && (
                    <div className="relative flex gap-2">
                        <button
                            type="submit"
                            className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
                            onClick={() => {
                                doUploadNonConflicting()
                                doUpdate([firstConflict])
                                switch (firstConflict.new.type) {
                                    case backendModule.AssetType.file: {
                                        setConflictingFiles(oldConflicts => oldConflicts.slice(1))
                                        break
                                    }
                                    case backendModule.AssetType.project: {
                                        setConflictingProjects(oldConflicts =>
                                            oldConflicts.slice(1)
                                        )
                                        break
                                    }
                                }
                            }}
                        >
                            Update
                        </button>
                        <button
                            type="submit"
                            className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
                            onClick={() => {
                                doUploadNonConflicting()
                                doRename([firstConflict])
                                switch (firstConflict.new.type) {
                                    case backendModule.AssetType.file: {
                                        setConflictingFiles(oldConflicts => oldConflicts.slice(1))
                                        break
                                    }
                                    case backendModule.AssetType.project: {
                                        setConflictingProjects(oldConflicts =>
                                            oldConflicts.slice(1)
                                        )
                                        break
                                    }
                                }
                            }}
                        >
                            Rename New File
                        </button>
                    </div>
                )}
                {[otherFilesText, otherProjectsText].join(' ')}
                <div className="relative flex gap-2">
                    <button
                        type="submit"
                        className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
                        onClick={() => {
                            doUploadNonConflicting()
                            doUpdate([...conflictingFiles, ...conflictingProjects])
                        }}
                    >
                        {count === 1 ? 'Update' : 'Update All'}
                    </button>
                    <button
                        type="submit"
                        className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
                        onClick={() => {
                            doUploadNonConflicting()
                            doRename([...conflictingFiles, ...conflictingProjects])
                        }}
                    >
                        {count === 1 ? 'Rename New File' : 'Rename New Files'}
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
