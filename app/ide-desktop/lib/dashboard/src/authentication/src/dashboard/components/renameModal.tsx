/** @file Modal for confirming delete of any type of asset. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import Modal from './modal'

export interface RenameModalProps {
    assetType: string
    name: string
    doRename: (newName: string) => Promise<void>
    onSuccess: () => void
}

function RenameModal(props: RenameModalProps) {
    const { assetType, name, doRename, onSuccess } = props
    const { unsetModal } = modalProvider.useSetModal()
    const [newName, setNewName] = react.useState<string | null>(null)
    return (
        <Modal className="bg-opacity-90">
            <form
                className="relative bg-white shadow-soft rounded-lg w-96 p-2"
                onClick={event => {
                    event.stopPropagation()
                }}
            >
                <button type="button" className="absolute right-0 top-0 m-2" onClick={unsetModal}>
                    {svg.CLOSE_ICON}
                </button>
                What do you want to rename the {assetType} '{name}' to?
                <div className="m-2">
                    <label className="w-1/3" htmlFor="uploaded_file_name">
                        File name
                    </label>
                    <input
                        id="uploaded_file_name"
                        type="text"
                        required
                        className="border-primary bg-gray-200 rounded-full w-2/3 px-2 mx-2"
                        onChange={event => {
                            setNewName(event.target.value)
                        }}
                        defaultValue={newName ?? ''}
                    />
                </div>
                <div className="m-1">
                    <div
                        className="hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-1"
                        onClick={async () => {
                            if (newName == null) {
                                toast.error('Please provide a new name.')
                            } else {
                                unsetModal()
                                await toast.promise(doRename(newName), {
                                    loading: `Deleting ${assetType}...`,
                                    success: `Deleted ${assetType}.`,
                                    error: `Could not delete ${assetType}.`,
                                })
                                onSuccess()
                            }
                        }}
                    >
                        Rename
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

export default RenameModal
