/** @file Form to create a project. */
import * as react from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as modalProvider from '../../providers/modal'
import * as templates from './templates'

import CreateForm, * as createForm from './createForm'
import Dropdown from './dropdown'

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

    const onSubmit = async (event: react.FormEvent) => {
        event.preventDefault()
        unsetModal()
        const finalName = name ?? defaultName
        const templateText = templateId == null ? '' : `from template '${templateId}'`
        await toast.promise(
            backend.createProject({
                parentDirectoryId: directoryId,
                projectName: name ?? defaultName,
                projectTemplateName: templateId,
            }),
            {
                loading: `Creating project '${finalName}'${templateText}...`,
                success: `Sucessfully created project '${finalName}'${templateText}.`,
                // This is UNSAFE, as the original function's parameter is of type `any`.
                error: (promiseError: Error) =>
                    `Error creating project '${finalName}'${templateText}: ${promiseError.message}`,
            }
        )
        onSuccess()
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
                    value={name ?? defaultName}
                    onChange={event => {
                        setName(event.target.value)
                    }}
                />
            </div>
            <div className="flex flex-row flex-nowrap m-1">
                <label className="inline-block flex-1 grow m-1" htmlFor="project_template_name">
                    Template
                </label>
                <Dropdown
                    className="flex-1 grow-2 px-2 m-1"
                    optionsClassName="-mx-2"
                    items={['None', ...templates.TEMPLATES.map(item => item.title)]}
                    onChange={newTemplateTitle => {
                        const newTemplateId =
                            templates.TEMPLATES.find(
                                template => template.title === newTemplateTitle
                            )?.id ?? null
                        setTemplateId(newTemplateId)
                        if (name == null) {
                            setDefaultName(getNewProjectName(newTemplateId))
                        }
                    }}
                />
            </div>
        </CreateForm>
    )
}

export default ProjectCreateForm
