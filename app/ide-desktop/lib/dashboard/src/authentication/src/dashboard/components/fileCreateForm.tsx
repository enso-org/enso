/** @file Form to create a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as dashboard from './dashboard'
import * as error from '../../error'
import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

export interface FileCreateFormProps extends dashboard.CreateFormProps {}

function FileCreateForm(props: FileCreateFormProps) {
    const { backend, directoryId, onSuccess } = props
    const { unsetModal } = modalProvider.useSetModal()
    const [name, setName] = react.useState<string | null>(null)
    const [file, setFile] = react.useState<File | null>(null)

    async function onSubmit(event: react.FormEvent) {
        event.preventDefault()
        if (file == null) {
            // TODO[sb]: Uploading a file may be a mistake when creating a new file.
            toast.error('Please select a file to upload.')
        } else {
            unsetModal()
            await toast
                .promise(
                    backend.uploadFile(
                        {
                            parentDirectoryId: directoryId,
                            fileName: name ?? file.name,
                        },
                        file
                    ),
                    {
                        loading: 'Uploading file...',
                        success: 'Sucessfully uploaded file.',
                        error: error.unsafeIntoErrorMessage,
                    }
                )
                .then(onSuccess)
        }
    }

    return (
        <form className="bg-white shadow-soft rounded-lg w-60" onSubmit={onSubmit}>
            <button type="button" className="absolute right-0 m-2" onClick={unsetModal}>
                {svg.CLOSE_ICON}
            </button>
            <h2 className="inline-block font-semibold m-2">New File</h2>
            <div className="flex flex-row flex-nowrap m-1">
                <label className="inline-block flex-1 grow m-1" htmlFor="file_name">
                    Name
                </label>
                <input
                    id="file_name"
                    type="text"
                    size={1}
                    className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                    onChange={event => {
                        setName(event.target.value)
                    }}
                    defaultValue={name ?? file?.name ?? ''}
                />
            </div>
            <div className="flex flex-row flex-nowrap m-1">
                <div className="inline-block flex-1 grow m-1">File</div>
                <div className="inline-block bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1">
                    <label className="bg-transparent rounded-full w-full" htmlFor="file_file">
                        <div className="inline-block bg-gray-300 hover:bg-gray-400 rounded-l-full px-2 -ml-2">
                            <u>𐌣</u>
                        </div>
                        <div className="inline-block px-2 -mr-2">
                            {file?.name ?? 'No file chosen'}
                        </div>
                    </label>
                    <input
                        id="file_file"
                        type="file"
                        className="hidden"
                        onChange={event => {
                            setFile(event.target.files?.[0] ?? null)
                        }}
                    />
                </div>
            </div>
            <input
                type="submit"
                className="hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-2"
                value="Create"
            />
        </form>
    )
}

export default FileCreateForm
