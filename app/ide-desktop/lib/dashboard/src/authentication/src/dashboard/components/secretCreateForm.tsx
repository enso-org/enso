/** @file Form to create a project. */
import * as React from 'react'
import toast from 'react-hot-toast'

import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as error from '../../error'
import * as modalProvider from '../../providers/modal'
import CreateForm, * as createForm from './createForm'

// ========================
// === SecretCreateForm ===
// ========================

/** Props for a {@link SecretCreateForm}. */
export interface SecretCreateFormProps extends createForm.CreateFormPassthroughProps {
    directoryId: backendModule.DirectoryId
    onSuccess: () => void
}

/** A form to create a secret. */
function SecretCreateForm(props: SecretCreateFormProps) {
    const { directoryId, onSuccess, ...passThrough } = props
    const { backend } = backendProvider.useBackend()
    const { unsetModal } = modalProvider.useSetModal()

    const [name, setName] = React.useState<string | null>(null)
    const [value, setValue] = React.useState<string | null>(null)

    if (backend.type === backendModule.BackendType.local) {
        return <></>
    } else {
        const onSubmit = async (event: React.FormEvent) => {
            event.preventDefault()
            if (name == null || name === '') {
                toast.error('Please provide a secret name.')
            } else if (value == null) {
                // Secret value explicitly can be empty.
                toast.error('Please provide a secret value.')
            } else {
                unsetModal()
                await toast
                    .promise(
                        backend.createSecret({
                            parentDirectoryId: directoryId,
                            secretName: name,
                            secretValue: value,
                        }),
                        {
                            loading: 'Creating secret...',
                            success: 'Sucessfully created secret.',
                            error: error.unsafeIntoErrorMessage,
                        }
                    )
                    .then(onSuccess)
            }
        }

        return (
            <CreateForm title="New Secret" onSubmit={onSubmit} {...passThrough}>
                <div className="flex flex-row flex-nowrap m-1">
                    <label className="inline-block flex-1 grow m-1" htmlFor="project_name">
                        Name
                    </label>
                    <input
                        id="project_name"
                        type="text"
                        size={1}
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
                        size={1}
                        className="bg-gray-200 rounded-full flex-1 grow-2 px-2 m-1"
                        onChange={event => {
                            setValue(event.target.value)
                        }}
                    />
                </div>
            </CreateForm>
        )
    }
}

export default SecretCreateForm
