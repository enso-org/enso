/** @file Modal for confirming delete of any type of asset. */
import toast from 'react-hot-toast'

import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import Modal from './modal'

// =================
// === Component ===
// =================

export interface ConfirmDeleteModalProps {
    assetType: string
    name: string
    doDelete: () => Promise<void>
    onSuccess: () => void
}

function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
    const { assetType, name, doDelete, onSuccess } = props
    const { unsetModal } = modalProvider.useSetModal()

    return (
        <Modal centered className="bg-opacity-90">
            <form
                className="relative bg-white shadow-soft rounded-lg w-96 p-2"
                onClick={event => {
                    event.stopPropagation()
                }}
            >
                <button type="button" className="absolute right-0 top-0 m-2" onClick={unsetModal}>
                    {svg.CLOSE_ICON}
                </button>
                Are you sure you want to delete the {assetType} '{name}'?
                <div className="m-1">
                    <div
                        className="hover:cursor-pointer inline-block text-white bg-red-500 rounded-full px-4 py-1 m-1"
                        onClick={async () => {
                            unsetModal()
                            await toast.promise(doDelete(), {
                                loading: `Deleting ${assetType}...`,
                                success: `Deleted ${assetType}.`,
                                error: `Could not delete ${assetType}.`,
                            })
                            onSuccess()
                        }}
                    >
                        Delete
                    </div>
                    <div
                        className="hover:cursor-pointer inline-block bg-gray-200 rounded-full px-4 py-1 m-1"
                        onClick={unsetModal}
                    >
                        Cancel
                    </div>
                </div>
            </form>
        </Modal>
    )
}

export default ConfirmDeleteModal
