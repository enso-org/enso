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
    onSuccess: () => void
}

/** A modal for renaming an asset. */
function RenameModal(props: RenameModalProps) {
    const { assetType, name, namePattern, title, doRename, onSuccess } = props
    const { unsetModal } = modalProvider.useSetModal()

    const [isSubmitting, setIsSubmitting] = react.useState(false)
    const [newName, setNewName] = react.useState<string | null>(null)

    const onSubmit = async (event: React.FormEvent<HTMLFormElement>) => {
        event.preventDefault()
        if (newName == null) {
            toast.error('Please provide a new name.')
        } else if (!isSubmitting) {
            try {
                setIsSubmitting(true)
                await toast.promise(doRename(newName), {
                    loading: `Renaming ${assetType}...`,
                    success: `Renamed ${assetType}.`,
                    // This is UNSAFE, as the original function's parameter is of type `any`.
                    error: (promiseError: Error) =>
                        `Error renaming ${assetType}: ${promiseError.message}`,
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
                onSubmit={onSubmit}
                className="relative bg-white shadow-soft rounded-lg w-96 p-2"
            >
                <button type="button" className="absolute right-0 top-0 m-2" onClick={unsetModal}>
                    <img src={CloseIcon} />
                </button>
                What do you want to rename the {assetType} '{name}' to?
                <div className="m-2">
                    <label className="w-1/3" htmlFor="renamed_file_name">
                        File name
                    </label>
                    <Input
                        autoFocus
                        required
                        // Never disabled, as disabling unfocuses the input.
                        id="renamed_file_name"
                        type="text"
                        pattern={namePattern}
                        title={title}
                        className="border-primary bg-gray-200 rounded-full w-2/3 px-2 mx-2"
                        defaultValue={newName ?? name}
                        setValue={setNewName}
                    />
                </div>
                <div className="m-1">
                    <button
                        type="submit"
                        disabled={isSubmitting}
                        className={`hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-1 ${
                            isSubmitting ? 'opacity-50' : ''
                        }`}
                    >
                        Rename
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

export default RenameModal
