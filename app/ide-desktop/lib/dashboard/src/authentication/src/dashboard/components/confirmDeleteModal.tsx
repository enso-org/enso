/** @file Modal for confirming delete of any type of asset. */
import * as react from 'react'
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
    onSuccess: () => void
}

/** A modal for confirming the deletion of an asset. */
function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
    const { assetType, name, doDelete, onSuccess } = props
    const { unsetModal } = modalProvider.useSetModal()

    const [isSubmitting, setIsSubmitting] = react.useState(false)

    const onSubmit = async () => {
        if (!isSubmitting) {
            try {
                setIsSubmitting(true)
                await toast.promise(doDelete(), {
                    loading: `Deleting ${assetType}...`,
                    success: `Deleted ${assetType}.`,
                    error: `Could not delete ${assetType}.`,
                })
                unsetModal()
                onSuccess()
            } finally {
                setIsSubmitting(false)
            }
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
                <button type="button" className="absolute right-0 top-0 m-2" onClick={unsetModal}>
                    <img src={CloseIcon} />
                </button>
                Are you sure you want to delete the {assetType} '{name}'?
                <div className="m-1">
                    <button
                        type="submit"
                        disabled={isSubmitting}
                        className={`hover:cursor-pointer inline-block text-white bg-red-500 rounded-full px-4 py-1 m-1 ${
                            isSubmitting ? 'opacity-50' : ''
                        }`}
                    >
                        Delete
                    </button>
                    <button
                        type="button"
                        disabled={isSubmitting}
                        className={`hover:cursor-pointer inline-block bg-gray-200 rounded-full px-4 py-1 m-1 ${
                            isSubmitting ? 'opacity-50' : ''
                        }`}
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
