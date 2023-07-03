/** @file Modal for confirming delete of any type of asset. */
import * as react from 'react'
import toast from 'react-hot-toast'

import CloseIcon from 'enso-assets/close.svg'

import * as modalProvider from '../../providers/modal'

import Input from './input'
import Modal from './modal'

// ===================
// === RenameModal ===
// ===================

/** Props for a {@link RenameModal}. */
export interface RenameModalProps {
    assetType: string
    name: string
    namePattern?: string
    title?: string
    doRename: (newName: string) => Promise<void>
    onComplete: () => void
}

/** A modal for renaming an asset. */
function RenameModal(props: RenameModalProps) {
    const { assetType, name, namePattern, title, doRename, onComplete } = props
    const { unsetModal } = modalProvider.useSetModal()

    const [newName, setNewName] = react.useState<string | null>(null)

    const onSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
        event.preventDefault()
        if (newName == null) {
            toast.error('Please provide a new name.')
        } else {
            unsetModal()
            try {
                await toast.promise(doRename(newName), {
                    loading: `Renaming ${assetType} '${name}' to '${newName}'...`,
                    success: `Renamed ${assetType} '${name}' to '${newName}'.`,
                    // This is UNSAFE, as the original function's parameter is of type `any`.
                    error: (promiseError: Error) =>
                        `Error renaming ${assetType} '${name}' to '${newName}': ${promiseError.message}`,
                })
            } finally {
                onComplete()
            }
        }
    }

    return (
        <Modal centered className="bg-opacity-90">
            <form
                onClick={event => {
                    event.stopPropagation()
                }}
                onSubmit={onSubmit}
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
                    What do you want to rename the {assetType} '{name}' to?
                </div>
                <div className="m-2">
                    <Input
                        autoFocus
                        required
                        // Never disabled, as disabling unfocuses the input.
                        id="renamed_asset_name"
                        type="text"
                        pattern={namePattern}
                        title={title}
                        className="border-primary bg-gray-200 rounded-full w-full px-2"
                        defaultValue={newName ?? name}
                        setValue={setNewName}
                    />
                </div>
                <div className="m-1">
                    <button
                        type="submit"
                        className="hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-1"
                    >
                        Rename
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

export default RenameModal
