/** @file Styled input element. */
import * as React from 'react'

// =================
// === Constants ===
// =================

/** The delay to wait before checking input validity. */
const DEBOUNCE_MS = 1000

// =============
// === Input ===
// =============

/** Props for an {@link Input}. */
export interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
    value: string
    error?: string
    validate?: boolean
    setValue: (value: string) => void
    shouldReportValidityRef?: React.MutableRefObject<boolean>
}

/** A component for authentication from inputs, with preset styles. */
export default function Input(props: InputProps) {
    const {
        setValue,
        error,
        validate = false,
        shouldReportValidityRef,
        onChange,
        onBlur,
        ...passThrough
    } = props
    const [reportTimeoutHandle, setReportTimeoutHandle] = React.useState<number | null>(null)
    const [hasReportedValidity, setHasReportedValidity] = React.useState(false)
    const [wasJustBlurred, setWasJustBlurred] = React.useState(false)
    return (
        <input
            {...passThrough}
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
                        currentTarget.setCustomValidity(
                            currentTarget.checkValidity() ||
                                shouldReportValidityRef?.current === false
                                ? ''
                                : error
                        )
                    }
                    if (hasReportedValidity) {
                        if (
                            shouldReportValidityRef?.current === false ||
                            currentTarget.checkValidity()
                        ) {
                            setHasReportedValidity(false)
                        }
                    } else {
                        setReportTimeoutHandle(
                            window.setTimeout(() => {
                                if (
                                    shouldReportValidityRef?.current !== false &&
                                    !currentTarget.reportValidity()
                                ) {
                                    setHasReportedValidity(true)
                                }
                            }, DEBOUNCE_MS)
                        )
                    }
                }
            }}
            onBlur={
                validate
                    ? event => {
                          onBlur?.(event)
                          if (wasJustBlurred) {
                              setHasReportedValidity(false)
                          } else {
                              const currentTarget = event.currentTarget
                              if (shouldReportValidityRef?.current !== false) {
                                  if (!currentTarget.reportValidity()) {
                                      event.preventDefault()
                                  }
                              }
                              setWasJustBlurred(true)
                          }
                      }
                    : onBlur
            }
            className="text-sm sm:text-base placeholder-gray-500 pl-10 pr-4 rounded-lg border border-gray-400 w-full py-2 focus:outline-none focus:border-blue-400"
        />
    )
}
