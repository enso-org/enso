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
                ref={element => {
                    element?.focus()
                }}
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
                    className="relative shadow-soft rounded-2xl w-96 px-4 py-2"
                >
                    <h1 className="text-base font-semibold">New Data Connector</h1>
                    <div className="m-2">Name</div>
                    <input
                        value={name}
                        onChange={() => {
                            setName(name)
                        }}
                    ></input>
                    <div className="m-2">Value</div>
                    <input
                        value={value}
                        onChange={() => {
                            setValue(value)
                        }}
                    ></input>
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
