/** @file Form to create a project. */
import * as react from 'react'
import * as toast from 'react-hot-toast'

import * as backendModule from '../service'
import * as fileInfo from '../../fileInfo'

export interface ProjectCreateFormProps {
    backend: backendModule.Backend
    directoryId: backendModule.DirectoryId
}

function FileCreateForm(props: ProjectCreateFormProps) {
    const { backend, directoryId } = props
    const [name, setName] = react.useState<string | null>(null)
    const [file, setFile] = react.useState<File | null>(null)

    async function onSubmit(event: react.FormEvent) {
        event.preventDefault()
        if (file == null) {
            // FIXME[sb]: Uploading a file may be a mistake when creating a new file.
            toast.toast.error('Please select a file to upload.')
        } else if (!name) {
            toast.toast.error('Please provide a file name.')
        } else {
            await backend.uploadFile(
                {
                    parentDirectoryId: directoryId,
                    fileName: name,
                },
                file
            )
            close()
        }
    }

    return (
        <form className="bg-white shadow-soft rounded-lg w-80" onSubmit={onSubmit}>
            <h2 className="inline-block font-semibold m-2">New File</h2>
            <div className="flex flex-row flex-nowrap m-1">
                <label className="inline-block flex-1 grow m-1" htmlFor="file_name">
                    Name
                </label>
                <input
                    id="file_name"
                    type="text"
                    className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                    onChange={event => {
                        setName(event.target.value)
                    }}
                />
            </div>
            <div className="flex flex-row flex-nowrap m-1">
                <label className="inline-block flex-1 grow m-1" htmlFor="file">
                    File
                </label>
                <span className="flex-1 grow-2 m-1">
                    {file?.name ?? 'No file selected'}
                    {file ? ` (${fileInfo.toReadableSize(file.size)})` : ''}
                </span>
                <input
                    id="file"
                    type="text"
                    className="hidden"
                    onChange={event => {
                        setName(name ?? event.target.files?.[0]?.name ?? '')
                        setFile(event.target.files?.[0] ?? null)
                    }}
                />
            </div>
            <input
                type="submit"
                className="inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-2"
                value="Create"
            />
        </form>
    )
}

export default FileCreateForm
