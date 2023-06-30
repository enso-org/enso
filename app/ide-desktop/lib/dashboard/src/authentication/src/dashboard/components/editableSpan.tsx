/** @file A text `<span>` which turns into an `input` when desired. */
import * as React from 'react'

import CrossIcon from 'enso-assets/cross.svg'
import TickIcon from 'enso-assets/tick.svg'

import * as shortcuts from '../shortcuts'

// ====================
// === EditableSpan ===
// ====================

/** Props of an {@link EditableSpan} that are passed through to the base element. */
type EditableSpanPassthroughProps = JSX.IntrinsicElements['input'] & JSX.IntrinsicElements['span']

/** Props for an {@link EditableSpan}. */
export interface EditableSpanProps extends Omit<EditableSpanPassthroughProps, 'onSubmit'> {
    editable?: boolean
    onSubmit: (value: string) => void
    onCancel: () => void
    inputPattern?: string
    inputTitle?: string
    children: string
}

/** A `<span>` that can turn into an `<input type="text">`. */
function EditableSpan(props: EditableSpanProps) {
    const {
        editable = false,
        children,
        onSubmit,
        onCancel,
        inputPattern,
        inputTitle,
        ...passthroughProps
    } = props

    // This is incorrect, but SAFE, as the value is always set by the time it is used.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const inputRef = React.useRef<HTMLInputElement>(null!)

    if (editable) {
        return (
            <form
                className="flex grow"
                onSubmit={event => {
                    event.preventDefault()
                    onSubmit(inputRef.current.value)
                }}
            >
                <input
                    ref={inputRef}
                    autoFocus
                    type="text"
                    size={1}
                    defaultValue={children}
                    onBlur={event => event.currentTarget.form?.requestSubmit()}
                    onKeyUp={event => {
                        if (
                            shortcuts.SHORTCUT_REGISTRY.matchesKeyboardAction(
                                shortcuts.KeyboardAction.cancelEditName,
                                event
                            )
                        ) {
                            onCancel()
                        }
                    }}
                    {...(inputPattern != null ? { pattern: inputPattern } : {})}
                    {...(inputTitle != null ? { title: inputTitle } : {})}
                    {...passthroughProps}
                />
                <button type="submit" className="mx-0.5">
                    <img src={TickIcon} />
                </button>
                <button
                    className="mx-0.5"
                    onClick={event => {
                        event.stopPropagation()
                        onCancel()
                    }}
                >
                    <img src={CrossIcon} />
                </button>
            </form>
        )
    } else {
        return <span {...passthroughProps}>{children}</span>
    }
}

export default EditableSpan
