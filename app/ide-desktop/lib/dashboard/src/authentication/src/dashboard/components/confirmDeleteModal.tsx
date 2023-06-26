/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import CloseIcon from 'enso-assets/close.svg'

import * as errorModule from '../../error'
import * as modalProvider from '../../providers/modal'
import * as toastPromise from '../toastPromise'

import Modal from './modal'

// =================
// === Component ===
// =================

/** Props for a {@link ConfirmDeleteModal}. */
export interface ConfirmDeleteModalProps {
    assetType: string
    /** Must fit in the sentence "Are you sure you want to delete <description>"? */
    description: string
    doDelete: () => Promise<void>
}

/** A modal for confirming the deletion of an asset. */
function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
    const { assetType, description, doDelete } = props
    const { unsetModal } = modalProvider.useSetModal()

    const onSubmit = async () => {
        unsetModal()
        await toastPromise.toastPromise(doDelete(), {
            loading: `Deleting ${assetType}...`,
            success: `Deleted ${assetType}.`,
            error: error =>
                `Could not delete ${assetType}: ${
                    errorModule.tryGetMessage(error) ?? 'unknown error'
                }`,
        })
    }

    return (
        <Modal centered className="bg-opacity-90">
            <form
                onClick={event => {
                    event.stopPropagation()
                }}
                onSubmit={async event => {
                    event.preventDefault()
                    // Consider not calling `onSubmit()` here to make it harder to accidentally
                    // delete an important asset.
                    await onSubmit()
                }}
                className="relative bg-white shadow-soft rounded-lg w-96 p-2"
            >
                <button type="button" className="absolute right-0 top-0 m-2" onClick={unsetModal}>
                    <img src={CloseIcon} />
                </button>
                Are you sure you want to delete {description}?
                <div className="m-1">
                    <button
                        type="submit"
                        className="hover:cursor-pointer inline-block text-white bg-red-500 rounded-full px-4 py-1 m-1"
                    >
                        Delete
                    </button>
                    <button
                        type="button"
                        className="hover:cursor-pointer inline-block bg-gray-200 rounded-full px-4 py-1 m-1"
                        onClick={unsetModal}
                    >
                        Cancel
                    </button>
                </div>
            </form>
        </Modal>
    )
}

export default ConfirmDeleteModal
