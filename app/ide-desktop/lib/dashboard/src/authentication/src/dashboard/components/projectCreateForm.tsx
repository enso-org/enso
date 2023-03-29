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

function ProjectCreateForm(props: ProjectCreateFormProps) {
    const { backend, directoryId, onSuccess, close } = props
    const [name, setName] = react.useState<string | null>(null)
    const [template, setTemplate] = react.useState<string | null>(null)

    async function onSubmit(event: react.FormEvent) {
        event.preventDefault()
        if (name == null) {
            toast.toast.error('Please provide a project name.')
        } else {
            await backend.createProject({
                parentDirectoryId: directoryId,
                projectName: name,
                projectTemplateName: template,
            })
            onSuccess()
            close()
        }
    }

    return (
        <form className="bg-white shadow-soft rounded-lg w-80" onSubmit={onSubmit}>
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
                className="inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-2"
                value="Create"
            />
        </form>
    )
}

export default ProjectCreateForm
