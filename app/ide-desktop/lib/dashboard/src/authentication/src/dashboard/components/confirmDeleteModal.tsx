/** @file Modal for confirming delete of any type of asset. */
import toast from 'react-hot-toast'

import * as dashboard from './dashboard'
import * as svg from '../../components/svg'

export interface ConfirmDeleteModalProps {
    assetType: string
    name: string
    doDelete: () => Promise<void>
}

// This is not a component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax
function confirmDeleteModal(props: ConfirmDeleteModalProps) {
    return function ConfirmDeleteModal(dashboardProps: dashboard.ModalProps) {
        const { assetType, name, doDelete } = props
        const { close, onSuccess } = dashboardProps
        return (
            <form
                className="relative bg-white shadow-soft rounded-lg w-96 p-2"
                onClick={event => {
                    event.stopPropagation()
                }}
            >
                <button type="button" className="absolute right-0 top-0 m-2" onClick={close}>
                    {svg.CLOSE_ICON}
                </button>
                Are you sure you want to delete the {assetType} '{name}'?
                <div className="m-1">
                    <div
                        className="hover:cursor-pointer inline-block text-white bg-red-500 rounded-full px-4 py-1 m-1"
                        onClick={async () => {
                            close()
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
                        onClick={close}
                    >
                        Cancel
                    </div>
                </div>
            </form>
        )
    }
}

export default confirmDeleteModal
