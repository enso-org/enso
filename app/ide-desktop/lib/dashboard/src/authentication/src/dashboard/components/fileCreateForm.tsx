/** @file Form to create a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as error from '../../error'
import * as modalProvider from '../../providers/modal'
import * as platform from '../../platform'
import CreateForm, * as createForm from './createForm'

// ======================
// === FileCreateForm ===
// ======================

/** Props for a {@link FileCreateForm}. */
export interface FileCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

/** A form to create a file. */
function FileCreateForm(props: FileCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()
    const [name, setName] = react.useState<string | null>(null)
    const [file, setFile] = react.useState<File | null>(null)

    if (backend.platform === platform.Platform.desktop) {
        return <></>
    } else {
        const onSubmit = async (event: react.FormEvent) => {
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
            <CreateForm title="New File" onSubmit={onSubmit} {...passThrough}>
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
            </CreateForm>
        )
    }
}

export default FileCreateForm
