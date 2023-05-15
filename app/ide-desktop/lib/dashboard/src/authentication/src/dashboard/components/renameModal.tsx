/** @file Modal for confirming delete of any type of asset. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import Modal from './modal'

export interface RenameModalProps {
    assetType: string
    name: string
    namePattern?: string
    title?: string
    doRename: (newName: string) => Promise<void>
    onSuccess: () => void
}

function RenameModal(props: RenameModalProps) {
    const { assetType, name, namePattern, title, doRename, onSuccess } = props
    const { unsetModal } = modalProvider.useSetModal()

    const [newName, setNewName] = react.useState<string | null>(null)

    const onSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
        event.preventDefault()
        if (newName == null) {
            toast.error('Please provide a new name.')
        } else {
            unsetModal()
            await toast.promise(doRename(newName), {
                loading: `Renaming ${assetType}...`,
                success: `Renamed ${assetType}.`,
                // This is UNSAFE, as the original function's parameter is of type `any`.
                error: (promiseError: Error) =>
                    `Error renaming ${assetType}: ${promiseError.message}`,
            })
            onSuccess()
        }
    }

    console.log('what', namePattern, title)

    return (
        <Modal centered className="bg-opacity-90">
            <form
                onClick={event => {
                    event.stopPropagation()
                }}
                onSubmit={onSubmit}
                className="relative bg-white shadow-soft rounded-lg w-96 p-2"
            >
                <button type="button" className="absolute right-0 top-0 m-2" onClick={unsetModal}>
                    {svg.CLOSE_ICON}
                </button>
                What do you want to rename the {assetType} '{name}' to?
                <div className="m-2">
                    <label className="w-1/3" htmlFor="renamed_file_name">
                        File name
                    </label>
                    <input
                        autoFocus
                        id="renamed_file_name"
                        type="text"
                        required
                        pattern={namePattern}
                        title={title}
                        className="border-primary bg-gray-200 rounded-full w-2/3 px-2 mx-2"
                        onChange={event => {
                            setNewName(event.target.value)
                        }}
                        defaultValue={newName ?? name}
                    />
                </div>
                <div className="m-1">
                    <button
                        type="submit"
                        className="hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-1"
                    >
                        Rename
                    </button>
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
