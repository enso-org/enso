/** @file Modal for confirming delete of any type of asset. */

import toast from 'react-hot-toast'

export interface ConfirmDeleteModalProps {
    assetType: string
    name: string
    doDelete: () => Promise<void>
}

// This component MUST NOT use `useState` because it is not rendered directly.
function ConfirmDeleteModal(props: ConfirmDeleteModalProps) {
    const { assetType, name, doDelete } = props
    return (
        <form
            className="bg-white shadow-soft rounded-lg w-96 p-2"
            onClick={event => {
                event.stopPropagation()
            }}
        >
            Are you sure you want to delete the {assetType} '{name}'?
            <div className="m-1">
                <div
                    className="hover:cursor-pointer inline-block text-white bg-red-500 rounded-full px-4 py-1 m-1"
                    onClick={() => {
                        void toast.promise(doDelete(), {
                            loading: `Deleting ${assetType}...`,
                            success: `Deleted ${assetType}.`,
                            error: `Could not delete ${assetType}.`,
                        })
                    }}
                >
                    Delete
                </div>
                <div
                    className="hover:cursor-pointer inline-block bg-gray-200 rounded-full px-4 py-1 m-1"
                    onClick={close}
                >
                    Cancel
                </div>
            </div>
        </form>
    )
}

export default ConfirmDeleteModal
