/** @file Form to create a project. */
import * as react from 'react'
import * as toast from 'react-hot-toast'

import * as backendModule from '../service'

export interface ProjectCreateFormProps {
    backend: backendModule.Backend
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
    close: () => void
}

function SecretCreateForm(props: ProjectCreateFormProps) {
    const { backend, directoryId, onSuccess, close } = props
    const [name, setName] = react.useState<string | null>(null)
    const [value, setValue] = react.useState<string | null>(null)

    async function onSubmit(event: react.FormEvent) {
        event.preventDefault()
        if (!name) {
            toast.toast.error('Please provide a secret name.')
        } else if (value == null) {
            // Secret value explicitly can be empty.
            toast.toast.error('Please provide a secret value.')
        } else {
            await backend.createSecret({
                parentDirectoryId: directoryId,
                secretName: name,
                secretValue: value,
            })
            onSuccess()
            close()
        }
    }

    return (
        <form className="bg-white shadow-soft rounded-lg w-80" onSubmit={onSubmit}>
            <h2 className="inline-block font-semibold m-2">New Secret</h2>
            <div className="flex flex-row flex-nowrap m-1">
                <label className="inline-block flex-1 grow m-1" htmlFor="project_name">
                    Name
                </label>
                <input
                    id="project_name"
                    type="text"
                    className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                    onChange={event => {
                        setName(event.target.value)
                    }}
                />
            </div>
            <div className="flex flex-row flex-nowrap m-1">
                <label className="inline-block flex-1 grow m-1" htmlFor="secret_value">
                    Value
                </label>
                <input
                    id="secret_value"
                    type="text"
                    className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                    onChange={event => {
                        setValue(event.target.value)
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

export default SecretCreateForm
