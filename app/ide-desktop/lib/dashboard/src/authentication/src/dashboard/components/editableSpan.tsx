/** @file A text `<span>` which turns into an `input` when desired. */
import * as React from 'react'

/** Props of an {@link EditableSpan} that are passed through to the base element. */
type EditableSpanPassthroughProps = JSX.IntrinsicElements['input'] & JSX.IntrinsicElements['span']

/** Props for an {@link EditableSpan}. */
export interface EditableSpanProps extends EditableSpanPassthroughProps {
    editable?: boolean
    onBlur: React.FocusEventHandler<HTMLInputElement>
    onCancel: () => void
    inputPattern?: string
    inputTitle?: string
    children: string
}

/** A `<span>` that can turn into an `<input type="text">`. */
function EditableSpan(props: EditableSpanProps) {
    const { editable, children, onBlur, onCancel, inputPattern, inputTitle, ...passthroughProps } =
        props

    if (editable) {
        return (
            <input
                autoFocus
                type="text"
                defaultValue={children}
                onBlur={event => {
                    if (event.target.reportValidity()) {
                        onBlur(event)
                    }
                }}
                onKeyUp={event => {
                    if (!event.ctrlKey && !event.shiftKey && !event.altKey && !event.metaKey) {
                        if (event.key === 'Enter') {
                            event.currentTarget.blur()
                        } else if (event.key === 'Escape') {
                            onCancel()
                        }
                    }
                }}
                {...(inputPattern != null ? { pattern: inputPattern } : {})}
                {...(inputTitle != null ? { title: inputTitle } : {})}
                {...passthroughProps}
            />
        )
    } else {
        return <span {...passthroughProps}>{children}</span>
    }
}

export default EditableSpan
