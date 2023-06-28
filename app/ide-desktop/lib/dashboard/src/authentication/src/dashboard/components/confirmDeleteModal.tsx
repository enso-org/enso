/** @file Modal for confirming delete of any type of asset. */
import toast from 'react-hot-toast'

import CloseIcon from 'enso-assets/close.svg'

import * as modalProvider from '../../providers/modal'

import Modal from './modal'

// =================
// === Component ===
// =================

/** Props for a {@link ConfirmDeleteModal}. */
export interface ConfirmDeleteModalProps {
    assetType: string
    name: string
    doDelete: () => Promise<void>
    onComplete: () => void
}

/** A modal for confirming the deletion of an asset. */
function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
    const { assetType, name, doDelete, onComplete } = props
    const { unsetModal } = modalProvider.useSetModal()

    const onSubmit = async () => {
        unsetModal()
        try {
            await toast.promise(doDelete(), {
                loading: `Deleting ${assetType} '${name}'...`,
                success: `Deleted ${assetType} '${name}'.`,
                // This is UNSAFE, as the original function's parameter is of type `any`.
                error: (promiseError: Error) =>
                    `Error deleting ${assetType} '${name}': ${promiseError.message}`,
            })
        } finally {
            onComplete()
        }
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
                <div className="flex">
                    {/* Padding. */}
                    <div className="grow" />
                    <button type="button" onClick={unsetModal}>
                        <img src={CloseIcon} />
                    </button>
                </div>
                <div className="m-2">
                    Are you sure you want to delete the {assetType} '{name}'?
                </div>
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
