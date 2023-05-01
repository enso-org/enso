/** @file Form to create a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../service'
import * as error from '../../error'
import * as modalProvider from '../../providers/modal'
import CreateForm, * as createForm from './createForm'

export interface DirectoryCreateFormProps extends createForm.CreateFormPassthroughProps {
    backend: backendModule.Backend
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

function DirectoryCreateForm(props: DirectoryCreateFormProps) {
    const { backend, directoryId, onSuccess, ...passThrough } = props
    const { unsetModal } = modalProvider.useSetModal()
    const [name, setName] = react.useState<string | null>(null)

    async function onSubmit(event: react.FormEvent) {
        event.preventDefault()
        if (name == null) {
            toast.error('Please provide a directory name.')
        } else {
            unsetModal()
            await toast
                .promise(
                    backend.createDirectory({
                        parentId: directoryId,
                        title: name,
                    }),
                    {
                        loading: 'Creating directory...',
                        success: 'Sucessfully created directory.',
                        error: error.unsafeIntoErrorMessage,
                    }
                )
                .then(onSuccess)
        }
    }

    return (
        <CreateForm title="New Directory" onSubmit={onSubmit} {...passThrough}>
            <div className="flex flex-row flex-nowrap m-1">
                <label className="inline-block flex-1 grow m-1" htmlFor="directory_name">
                    Name
                </label>
                <input
                    id="directory_name"
                    type="text"
                    size={1}
                    className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                    onChange={event => {
                        setName(event.target.value)
                    }}
                />
            </div>
        </CreateForm>
    )
}

export default DirectoryCreateForm
