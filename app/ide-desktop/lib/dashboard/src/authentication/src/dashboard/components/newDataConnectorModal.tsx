/** @file Modal for confirming delete of any type of asset. */
import * as React from 'react'

import * as toastify from 'react-toastify'

import * as errorModule from '../../error'
import * as loggerProvider from '../../providers/logger'
import * as modalProvider from '../../providers/modal'
import Modal from './modal'

// =============================
// === NewDataConnectorModal ===
// =============================

/** Props for a {@link NewDataConnectorModal}. */
export interface NewDataConnectorModalProps {
    doCreate: (name: string, value: string) => void
}

/** A modal for confirming the deletion of an asset. */
export default function NewDataConnectorModal(props: NewDataConnectorModalProps) {
    const { doCreate } = props
    const logger = loggerProvider.useLogger()
    const { unsetModal } = modalProvider.useSetModal()

    const [name, setName] = React.useState('')
    const [value, setValue] = React.useState('')
    const canSubmit = Boolean(name && value)

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
                className="pointer-events-auto relative rounded-2xl"
                onKeyDown={event => {
                    if (event.key !== 'Escape') {
                        event.stopPropagation()
                    }
                }}
            >
                <div className="absolute h-full w-full rounded-2xl bg-frame-selected backdrop-blur-3xl" />
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
                    className="relative flex w-96 flex-col gap-2 rounded-2xl px-4 py-2"
                >
                    <h1 className="text-sm font-semibold">New Data Connector</h1>
                    <div className="flex">
                        <div className="h-6 w-12 py-1">Name</div>
                        <input
                            autoFocus
                            placeholder="Enter the name of the data connector"
                            className="h-6 grow rounded-full border border-black-a10 bg-transparent px-4 py-px leading-170"
                            onInput={event => {
                                setName(event.currentTarget.value)
                            }}
                        />
                    </div>
                    <div className="flex">
                        <div className="h-6 w-12 py-1">Value</div>
                        <input
                            placeholder="Enter the value of the data connector"
                            className="h-6 grow rounded-full border border-black-a10 bg-transparent px-4 py-px leading-170"
                            onInput={event => {
                                setValue(event.currentTarget.value)
                            }}
                        />
                    </div>
                    <div className="flex gap-2">
                        <button
                            disabled={!canSubmit}
                            type="submit"
                            className="inline-block rounded-full bg-invite px-4 py-1 text-white hover:cursor-pointer disabled:cursor-default disabled:opacity-50"
                        >
                            Create
                        </button>
                        <button
                            type="button"
                            className="inline-block rounded-full bg-frame-selected px-4 py-1 hover:cursor-pointer"
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
