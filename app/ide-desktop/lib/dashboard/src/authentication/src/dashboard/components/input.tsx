/** @file Input element with default event handlers. */
import * as React from 'react'

// =================
// === Constants ===
// =================

/** The delay to wait before checking input validity. */
const DEBOUNCE_MS = 1000

// =============
// === Input ===
// =============

/** Props for an `<input>` HTML element/ */
type InputAttributes = JSX.IntrinsicElements['input']

/** Props for an {@link Input}. */
export interface InputProps extends InputAttributes {
    value: string
    error?: string
    validate?: boolean
    setValue: (value: string) => void
}

/** A component for authentication from inputs, with preset styles. */
export default function Input(props: InputProps) {
    const { setValue, error, validate = false, onChange, onBlur, ...passthroughProps } = props
    const [reportTimeoutHandle, setReportTimeoutHandle] = React.useState<number | null>(null)
    const [hasReportedValidity, setHasReportedValidity] = React.useState(false)
    const [wasJustBlurred, setWasJustBlurred] = React.useState(false)
    return (
        <input
            {...passthroughProps}
            onChange={event => {
                onChange?.(event)
                setValue(event.target.value)
                setWasJustBlurred(false)
                if (validate) {
                    if (reportTimeoutHandle != null) {
                        window.clearTimeout(reportTimeoutHandle)
                    }
                    const currentTarget = event.currentTarget
                    if (error != null) {
                        currentTarget.setCustomValidity('')
                        currentTarget.setCustomValidity(currentTarget.checkValidity() ? '' : error)
                    }
                    if (hasReportedValidity) {
                        if (currentTarget.checkValidity()) {
                            setHasReportedValidity(false)
                        }
                    } else {
                        setReportTimeoutHandle(
                            window.setTimeout(() => {
                                if (!currentTarget.reportValidity()) {
                                    setHasReportedValidity(true)
                                }
                            }, DEBOUNCE_MS)
                        )
                    }
                }
            }}
            onBlur={event => {
                onBlur?.(event)
                if (validate) {
                    if (wasJustBlurred) {
                        setHasReportedValidity(false)
                    } else {
                        const currentTarget = event.currentTarget
                        setTimeout(() => {
                            currentTarget.reportValidity()
                        }, 0)
                    }
                }
                setWasJustBlurred(true)
                getSelection()?.empty()
            }}
        />
    )
}
