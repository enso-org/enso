/** @file Modal dialog to upload a file. */
import * as react from 'react'
import toast from 'react-hot-toast'

import CloseIcon from 'enso-assets/close.svg'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as fileInfo from '../../fileInfo'
import * as modalProvider from '../../providers/modal'

import Modal from './modal'

// =======================
// === UploadFileModal ===
// =======================

/** Props for an {@link UploadFileModal}. */
export interface UploadFileModalProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

/** A modal for uploading a file. */
function UploadFileModal(props: UploadFileModalProps) {
    const { directoryId, onSuccess } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [name, setName] = react.useState<string | null>(null)
    const [file, setFile] = react.useState<File | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        const onSubmit = async () => {
            if (file == null) {
                toast.error('Please select a file to upload.')
            } else {
                unsetModal()
                const toastId = toast.loading('Uploading file...')
                await backend.uploadFile(
                    {
                        parentDirectoryId: directoryId,
                        fileName: name ?? file.name,
                    },
                    file
                )
                toast.success('Sucessfully uploaded file.', { id: toastId })
                onSuccess()
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
                        await onSubmit()
                    }}
                    className="relative bg-white shadow-soft rounded-lg w-96 h-72 p-2"
                >
                    <button
                        type="button"
                        className="absolute right-0 top-0 m-2"
                        onClick={unsetModal}
                    >
                        <img src={CloseIcon} />
                    </button>
                    <div className="m-2">
                        <label className="w-1/3" htmlFor="uploaded_file_name">
                            File name
                        </label>
                        <input
                            required
                            id="uploaded_file_name"
                            type="text"
                            className="border-primary bg-gray-200 rounded-full w-2/3 px-2 mx-2"
                            onChange={event => {
                                setName(event.target.value)
                            }}
                            defaultValue={name ?? file?.name ?? ''}
                        />
                    </div>
                    <div className="m-2">
                        <label
                            htmlFor="uploaded_file"
                            className="hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1"
                        >
                            Select file
                        </label>
                    </div>
                    <div className="border border-primary rounded-md m-2">
                        <input
                            required
                            id="uploaded_file"
                            type="file"
                            className="hidden"
                            onChange={event => {
                                setFile(event.target.files?.[0] ?? null)
                            }}
                        />
                        <div className="inline-flex flex-row flex-nowrap w-full p-2">
                            <div className="grow">
                                <div>{file?.name ?? 'No file selected'}</div>
                                <div className="text-xs">
                                    {file ? fileInfo.toReadableSize(file.size) : '\u00a0'}
                                </div>
                            </div>
                            <div>{file && fileInfo.fileIcon()}</div>
                        </div>
                    </div>
                    <div className="m-1">
                        <div
                            className="hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-1"
                            onClick={onSubmit}
                        >
                            Upload
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
}

export default UploadFileModal
