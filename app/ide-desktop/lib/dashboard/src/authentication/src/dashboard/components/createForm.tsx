/** @file Base form to create an asset.
 * This should never be used directly, but instead should be wrapped in a component
 * that creates a specific asset type. */

import * as react from 'react'

import * as modalProvider from '../../providers/modal'
import * as svg from '../../components/svg'

import Modal from './modal'

// ==================
// === CreateForm ===
// ==================

/** The props that should also be in the wrapper component. */
export interface CreateFormPassthroughProps {
    left: number
    top: number
}

/** `CreateFormPassthroughProps`, plus props that should be defined in the wrapper component. */
export interface CreateFormProps extends CreateFormPassthroughProps, react.PropsWithChildren {
    title: string
    onSubmit: (event: react.FormEvent) => Promise<void>
}

/** A form to create an element. */
function CreateForm(props: CreateFormProps) {
    const { title, left, top, children, onSubmit: innerOnSubmit } = props
    const { unsetModal } = modalProvider.useSetModal()

    const onSubmit = async (event: react.FormEvent) => {
        event.preventDefault()
        await innerOnSubmit(event)
    }

    return (
        <Modal className="absolute overflow-hidden bg-opacity-25 w-full h-full top-0 left-0">
            <form
                style={{ left, top }}
                className="sticky bg-white shadow-soft rounded-lg w-64"
                onSubmit={onSubmit}
                onClick={event => {
                    event.stopPropagation()
                }}
            >
                <button type="button" className="absolute right-0 m-2" onClick={unsetModal}>
                    {svg.CLOSE_ICON}
                </button>
                <h2 className="inline-block font-semibold m-2">{title}</h2>
                {children}
                <input
                    type="submit"
                    className="hover:cursor-pointer inline-block text-white bg-blue-600 rounded-full px-4 py-1 m-2"
                    value="Create"
                />
            </form>
        </Modal>
    )
}

export default CreateForm
