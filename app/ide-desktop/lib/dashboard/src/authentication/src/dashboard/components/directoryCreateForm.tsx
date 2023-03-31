/** @file Form to create a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../service'
import * as svg from '../../components/svg'

export interface DirectoryCreateFormProps {
    backend: backendModule.Backend
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
    close: () => void
}

function DirectoryCreateForm(props: DirectoryCreateFormProps) {
    const { backend, directoryId, onSuccess, close } = props
    const [name, setName] = react.useState<string | null>(null)

    async function onSubmit(event: react.FormEvent) {
        event.preventDefault()
        if (name == null) {
            toast.error('Please provide a directory name.')
        } else {
            close()
            const toastId = toast.loading('Creating directory...')
            await backend.createDirectory({
                parentId: directoryId,
                title: name,
            })
            toast.success('Sucessfully created directory.', { id: toastId })
            onSuccess()
        }
    }

    return (
        <form className="relative bg-white shadow-soft rounded-lg w-80" onSubmit={onSubmit}>
            <button type="button" className="absolute right-0 m-2" onClick={close}>
                {svg.CLOSE_ICON}
            </button>
            <h2 className="inline-block font-semibold m-1">New Directory</h2>
            <div className="flex flex-row flex-nowrap m-1">
                <label className="inline-block flex-1 grow m-1" htmlFor="directory_name">
                    Name
                </label>
                <input
                    id="directory_name"
                    type="text"
                    className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                    onChange={event => {
                        setName(event.target.value)
                    }}
                />
            </div>
            <input
                type="submit"
                className="hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-2"
                value="Create"
            />
        </form>
    )
}

export default DirectoryCreateForm
