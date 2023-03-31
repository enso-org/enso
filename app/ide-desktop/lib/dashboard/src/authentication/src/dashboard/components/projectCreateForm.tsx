/** @file Form to create a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../service'
import * as svg from '../../components/svg'

export interface ProjectCreateFormProps {
    backend: backendModule.Backend
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
    close: () => void
}

function ProjectCreateForm(props: ProjectCreateFormProps) {
    const { backend, directoryId, onSuccess, close } = props
    const [name, setName] = react.useState<string | null>(null)
    const [template, setTemplate] = react.useState<string | null>(null)

    async function onSubmit(event: react.FormEvent) {
        event.preventDefault()
        if (name == null) {
            toast.error('Please provide a project name.')
        } else {
            close()
            const toastId = toast.loading('Creating project...')
            await backend.createProject({
                parentDirectoryId: directoryId,
                projectName: name,
                projectTemplateName: template,
            })
            toast.success('Sucessfully created project.', { id: toastId })
            onSuccess()
        }
    }

    return (
        <form className="bg-white shadow-soft rounded-lg w-80" onSubmit={onSubmit}>
            <button type="button" className="absolute right-0 m-2" onClick={close}>
                {svg.CLOSE_ICON}
            </button>
            <h2 className="inline-block font-semibold m-2">New Project</h2>
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
                {/* FIXME[sb]: Use the array of templates in a dropdown when it becomes available. */}
                <label className="inline-block flex-1 grow m-1" htmlFor="project_template_name">
                    Template
                </label>
                <input
                    id="project_template_name"
                    type="text"
                    className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                    onChange={event => {
                        setTemplate(event.target.value)
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

export default ProjectCreateForm
