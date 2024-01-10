/** @file A modal for creating a new label. */
import * as React from 'react'

import * as toastify from 'react-toastify'

import * as loggerProvider from '#/providers/LoggerProvider'
import * as modalProvider from '#/providers/ModalProvider'
import * as backend from '#/services/backend'
import * as errorModule from '#/utilities/error'

import ColorPicker from '#/components/ColorPicker'
import Modal from '#/components/Modal'

// =====================
// === NewLabelModal ===
// =====================

/** Props for a {@link NewLabelModal}. */
export interface NewLabelModalProps {
    labels: backend.Label[]
    eventTarget: HTMLElement
    doCreate: (value: string, color: backend.LChColor) => void
}

/** A modal for creating a new label. */
export default function NewLabelModal(props: NewLabelModalProps) {
    const { labels, eventTarget, doCreate } = props
    const logger = loggerProvider.useLogger()
    const { unsetModal } = modalProvider.useSetModal()
    const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])

    const labelNames = React.useMemo(
        () => new Set<string>(labels.map(label => label.value)),
        [labels]
    )
    const leastUsedColor = React.useMemo(() => backend.leastUsedColor(labels), [labels])

    const [value, setName] = React.useState('')
    const [color, setColor] = React.useState<backend.LChColor | null>(null)
    const canSubmit = Boolean(value && !labelNames.has(value))

    const onSubmit = () => {
        unsetModal()
        try {
            doCreate(value, color ?? leastUsedColor)
        } catch (error) {
            const message = errorModule.getMessageOrToString(error)
            toastify.toast.error(message)
            logger.error(message)
        }
    }

    return (
        <Modal className="absolute bg-dim">
            <form
                data-testid="new-label-modal"
                tabIndex={-1}
                style={{
                    left: position.left + window.scrollX,
                    top: position.top + window.scrollY,
                }}
                className="relative flex flex-col gap-2 rounded-2xl pointer-events-auto w-80 p-4 pt-2 before:inset-0 before:absolute before:rounded-2xl before:bg-frame-selected before:backdrop-blur-3xl before:w-full before:h-full"
                onKeyDown={event => {
                    if (event.key !== 'Escape') {
                        event.stopPropagation()
                    }
                }}
                onClick={event => {
                    event.stopPropagation()
                }}
                onSubmit={event => {
                    event.preventDefault()
                    // Consider not calling `onSubmit()` here to make it harder to accidentally
                    // delete an important asset.
                    onSubmit()
                }}
            >
                <h1 className="relative text-sm font-semibold">New Label</h1>
                <label className="relative flex">
                    <div className="w-12 h-6 py-1">Name</div>
                    <input
                        autoFocus
                        size={1}
                        placeholder="Enter the name of the label"
                        className={`grow bg-transparent border border-black/10 rounded-full leading-170 h-6 px-4 py-px ${
                            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                            color != null && color.lightness <= 50
                                ? 'text-tag-text placeholder-frame-selected'
                                : 'text-primary'
                        }`}
                        style={
                            color == null
                                ? {}
                                : {
                                      backgroundColor: backend.lChColorToCssColor(color),
                                  }
                        }
                        onInput={event => {
                            setName(event.currentTarget.value)
                        }}
                    />
                </label>
                <label
                    className="relative flex"
                    onClick={event => {
                        event.preventDefault()
                    }}
                >
                    <div className="w-12 h-6 py-1">Color</div>
                    <div className="grow flex items-center gap-1">
                        <ColorPicker setColor={setColor} />
                    </div>
                </label>
                <div className="relative flex gap-2">
                    <button
                        disabled={!canSubmit}
                        type="submit"
                        className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
                    >
                        Create
                    </button>
                    <button
                        type="button"
                        className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1"
                        onClick={unsetModal}
                    >
                        Cancel
                    </button>
                </div>
            </form>
        </Modal>
    )
}
