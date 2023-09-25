/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import * as errorModule from '../../error'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'

import Modal from './modal'

// =================
// === Component ===
// =================

/** Props for a {@link ConfirmDeleteModal}. */
export interface ConfirmDeleteModalProps {
    doCreate: (name: string, value: string) => void
}

/** A modal for confirming the deletion of an asset. */
export default function NewDataConnectorModal(props: ConfirmDeleteModalProps) {
    const { doCreate } = props
    const logger = loggerProvider.useLogger()
    const { unsetModal } = modalProvider.useSetModal()

    const [name, setName] = React.useState('')
    const [value, setValue] = React.useState('')

    const onSubmit = () => {
        unsetModal()
        try {
            doCreate(name, value)
        } catch (error) {
            const message = errorModule.getMessageOrToString(error)
            toastify.toast.error(message)
            logger.error(message)
        }
    }

    return (
        <Modal centered className="bg-dim">
            <div
                tabIndex={-1}
                className="relative rounded-2xl pointer-events-auto"
                onKeyDown={event => {
                    if (event.key !== 'Escape') {
                        event.stopPropagation()
                    }
                }}
            >
                <div className="absolute rounded-2xl bg-frame-selected backdrop-blur-3xl w-full h-full" />
                <form
                    onClick={event => {
                        event.stopPropagation()
                    }}
                    onSubmit={event => {
                        event.preventDefault()
                        // Consider not calling `onSubmit()` here to make it harder to accidentally
                        // delete an important asset.
                        onSubmit()
                    }}
                    className="relative flex flex-col shadow-soft gap-2 rounded-2xl w-96 px-4 py-2"
                >
                    <h1 className="text-sm font-semibold">New Data Connector</h1>
                    <div className="flex">
                        <div className="w-10 h-6 mx-2 py-1">Name</div>
                        <input
                            autoFocus
                            className="grow rounded-full h-6 px-4"
                            onInput={event => {
                                setName(event.currentTarget.value)
                            }}
                        />
                    </div>
                    <div className="flex">
                        <div className="w-10 h-6 mx-2 py-1">Value</div>
                        <input
                            className="grow rounded-full h-6 px-4"
                            onInput={event => {
                                setValue(event.currentTarget.value)
                            }}
                        />
                    </div>
                    <div className="m-1">
                        <button
                            type="submit"
                            className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 m-1"
                        >
                            Create
                        </button>
                        <button
                            type="button"
                            className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1 m-1"
                            onClick={unsetModal}
                        >
                            Cancel
                        </button>
                    </div>
                </form>
            </div>
        </Modal>
    )
}
