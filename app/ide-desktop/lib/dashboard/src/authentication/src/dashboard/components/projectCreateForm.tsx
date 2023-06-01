/** @file Form to create a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as error from '../../error'
import * as modalProvider from '../../providers/modal'
import * as templates from './templates'

import CreateForm, * as createForm from './createForm'

// =========================
// === ProjectCreateForm ===
// =========================

/** Props for a {@link ProjectCreateForm}. */
export interface ProjectCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    getNewProjectName: (templateId: string | null) => string
    onSuccess: () => void
}

/** A form to create a project. */
function ProjectCreateForm(props: ProjectCreateFormProps) {
    const { directoryId, getNewProjectName, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [defaultName, setDefaultName] = react.useState(() => getNewProjectName(null))
    const [name, setName] = react.useState<string | null>(null)
    const [templateId, setTemplateId] = react.useState<string | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        const onSubmit = async (event: react.FormEvent) => {
            event.preventDefault()
            if (name == null) {
                toast.error('Please provide a project name.')
            } else {
                unsetModal()
                await toast
                    .promise(
                        backend.createProject({
                            parentDirectoryId: directoryId,
                            projectName: name,
                            projectTemplateName: templateId,
                        }),
                        {
                            loading: 'Creating project...',
                            success: 'Sucessfully created project.',
                            error: error.unsafeIntoErrorMessage,
                        }
                    )
                    .then(onSuccess)
            }
        }

        return (
            <CreateForm title="New Project" onSubmit={onSubmit} {...passThrough}>
                <div className="flex flex-row flex-nowrap m-1">
                    <label className="inline-block flex-1 grow m-1" htmlFor="project_name">
                        Name
                    </label>
                    <input
                        id="project_name"
                        type="text"
                        size={1}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                        defaultValue={defaultName}
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
                    <select
                        id="project_template_name"
                        onChange={event => {
                            const templateId = event.target.value
                            setTemplateId(event.target.value)
                            if (name == null) {
                                setDefaultName(getNewProjectName(templateId))
                            }
                        }}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                    >
                        {templates.TEMPLATES.map(templateOption => (
                            <option value={templateOption.id}>{templateOption.title}</option>
                        ))}
                    </select>
                </div>
            </CreateForm>
        )
    }
}

export default ProjectCreateForm
