/** @file Form to create a project. */
import * as react from 'react'
import * as toast from 'react-hot-toast'

import * as backendModule from '../service'

export interface ProjectCreateFormProps {
    backend: backendModule.Backend
    directoryId: backendModule.DirectoryId | null
}

function ProjectCreateForm(props: ProjectCreateFormProps) {
    const { backend, directoryId } = props
    const [name, setName] = react.useState<string | null>(null)
    const [template, setTemplate] = react.useState<string | null>(null)

    function onSubmit() {
        if (name == null) {
            toast.toast.error('Please provide a project name.')
        } else {
            void backend.createProject({
                projectName: name,
                parentDirectoryId: directoryId,
                projectTemplateName: template,
            })
        }
    }

    return (
        <form onSubmit={onSubmit}>
            <h2 className="text-lg">New Project</h2>
            <label htmlFor="project_name">Name: </label>
            <input
                id="project_name"
                type="text"
                onChange={event => {
                    setName(event.target.value)
                }}
            />
            {/* FIXME[sb]: Use the array of templates in a dropdown when it becomes available. */}
            <label htmlFor="project_template_name">Name: </label>
            <input
                id="project_template_name"
                type="text"
                onChange={event => {
                    setTemplate(event.target.value)
                }}
            />
            <input type="submit" value="Create" />
        </form>
    )
}

export default ProjectCreateForm
