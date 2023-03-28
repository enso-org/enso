/** @file Modal dialog to upload a file. */
import * as react from 'react'
import * as toast from 'react-hot-toast'

import * as backendModule from '../service'
import * as fileInfo from '../../fileInfo'

export interface UploadFileModalProps {
    backend: backendModule.Backend
    directoryId: backendModule.DirectoryId
    closeModal: () => void
}

function UploadFileModal(props: UploadFileModalProps) {
    const { backend, directoryId, closeModal } = props

    const [uploadedFileName, setUploadedFileName] = react.useState<string>('')
    const [uploadedFile, setUploadedFile] = react.useState<File | null>(null)

    async function onSubmit() {
        if (uploadedFile == null) {
            toast.toast.error('Please upload a file.')
        } else if (!uploadedFileName) {
            toast.toast.error('Please name the uploaded file.')
        } else {
            await backend.uploadFile(
                {
                    fileName: uploadedFileName,
                    parentDirectoryId: directoryId,
                },
                uploadedFile
            )
            toast.toast.success('Sucessfully uploaded file.')
            closeModal()
        }
    }

    return (
        <form className="bg-white rounded-lg w-96 h-72 p-2">
            <div className="m-2">
                <label htmlFor="uploaded_file_name">File name</label>:{' '}
                <input
                    id="uploaded_file_name"
                    type="text"
                    required
                    className="border-primary bg-gray-100 rounded-full px-2 mx-2"
                    onChange={event => {
                        setUploadedFileName(event.target.value)
                    }}
                    defaultValue={uploadedFileName}
                />
            </div>
            <div className="m-2">
                <div className="inline-block text-white bg-blue-600 rounded-full px-4 py-1">
                    <label htmlFor="uploaded_file">Select file</label>
                </div>
            </div>
            <div className="border border-primary rounded-md m-2">
                <input
                    id="uploaded_file"
                    type="file"
                    className="hidden"
                    onChange={event => {
                        setUploadedFileName(
                            uploadedFileName || (event.target.files?.[0]?.name ?? '')
                        )
                        setUploadedFile(event.target.files?.[0] ?? null)
                    }}
                />
                <div className="inline-flex flex-row flex-nowrap w-full p-2">
                    <div className="grow">
                        <div>{uploadedFile?.name ?? 'No file selected'}</div>
                        <div className="text-xs">
                            {uploadedFile ? fileInfo.toReadableSize(uploadedFile.size) : '\u00a0'}
                        </div>
                    </div>
                    <div>
                        {uploadedFile ? (
                            fileInfo.fileIcon(fileInfo.fileExtension(uploadedFile.name))
                        ) : (
                            <></>
                        )}
                    </div>
                </div>
            </div>
            <div className="m-1">
                <div
                    className="inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-1"
                    onClick={onSubmit}
                >
                    Upload
                </div>
                <div
                    className="inline-block bg-gray-200 rounded-full px-4 py-1 m-1"
                    onClick={closeModal}
                >
                    Cancel
                </div>
            </div>
        </form>
    )
}

export default UploadFileModal
