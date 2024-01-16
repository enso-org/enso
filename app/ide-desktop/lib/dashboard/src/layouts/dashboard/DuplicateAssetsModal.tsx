/** @file A modal opened when uploaded assets. */
import * as React from 'react'

import * as modalProvider from '#/providers/ModalProvider'
import type * as backend from '#/services/backend'

import Modal from '#/components/Modal'

// =============
// === Types ===
// =============

/** An object containing the current asset, and the asset that is about to be uploaded,
 * that will conflict with the existing asset. */
export interface ConflictingAsset {
    current: backend.AnyAsset
    new: backend.AnyAsset
}

// =================================
// === UpdateOrRenameAssetsModal ===
// =================================

/** Props for a {@link DuplicateAssetsModal}. */
export interface DuplicateAssetsModalProps {
    conflictingAssets: ConflictingAsset[]
    doUpdate: () => void
    doRename: () => void
}

/** A modal for creating a new label. */
export default function DuplicateAssetsModal(props: DuplicateAssetsModalProps) {
    const { conflictingAssets, doUpdate, doRename } = props
    const { unsetModal } = modalProvider.useSetModal()

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
                {/* TODO: display current/new asset if there is one, otherwise show box */}
                <div className="relative flex gap-2">
                    <button
                        type="submit"
                        className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
                        onClick={doUpdate}
                    >
                        {conflictingAssets.length === 1 ? 'Update' : 'Update All'}
                    </button>
                    <button
                        type="submit"
                        className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
                        onClick={doRename}
                    >
                        {conflictingAssets.length === 1 ? 'Rename New File' : 'Rename New Files'}
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
