/** @file Styled input element. */
import * as React from 'react'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'

// =================
// === Constants ===
// =================

/** The delay to wait before checking input validity. */
const DEBOUNCE_MS = 1000

// =======================
// === ControlledInput ===
// =======================

/** Props for a {@link ControlledInput}. */
export interface ControlledInputProps extends Readonly<aria.InputProps> {
  readonly value: string
  readonly error?: string
  readonly validate?: boolean
  readonly setValue: (value: string) => void
  readonly shouldReportValidityRef?: React.MutableRefObject<boolean>
}

/** A component for authentication from inputs, with preset styles. */
export default function ControlledInput(props: ControlledInputProps) {
  const {
    setValue,
    error,
    validate = false,
    shouldReportValidityRef,
    onKeyDown,
    onChange,
    onBlur,
    ...inputProps
  } = props
  const [reportTimeoutHandle, setReportTimeoutHandle] = React.useState<number | null>(null)
  const [hasReportedValidity, setHasReportedValidity] = React.useState(false)
  const [wasJustBlurred, setWasJustBlurred] = React.useState(false)
  const focusChildProps = focusHooks.useFocusChild()

  return (
    <FocusRing>
      <aria.Input
        {...aria.mergeProps<aria.InputProps>()(inputProps, focusChildProps, {
          className:
            'w-full rounded-full border py-auth-input-y pl-auth-icon-container-w pr-auth-input-r text-sm placeholder-gray-500 transition-all duration-auth hover:bg-gray-100 focus:bg-gray-100',
          onKeyDown: event => {
            if (!event.isPropagationStopped()) {
              onKeyDown?.(event)
            }
          },
          onChange: event => {
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
                  currentTarget.checkValidity() || shouldReportValidityRef?.current === false
                    ? ''
                    : error
                )
              }
              if (hasReportedValidity) {
                if (shouldReportValidityRef?.current === false || currentTarget.checkValidity()) {
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
          },
          onBlur: validate
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
            : onBlur,
        })}
      />
    </FocusRing>
  )
}
