/** @file A text `<span>` which turns into an `input` when desired. */
import * as React from 'react'

import CrossIcon from 'enso-assets/cross.svg'
import TickIcon from 'enso-assets/tick.svg'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'

import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'

// ====================
// === EditableSpan ===
// ====================

/** Props for an {@link EditableSpan}. */
export interface EditableSpanProps {
  // This matches the capitalization of `data-` attributes in React.
  // eslint-disable-next-line @typescript-eslint/naming-convention
  readonly 'data-testid'?: string
  readonly className?: string
  readonly editable?: boolean
  readonly checkSubmittable?: (value: string) => boolean
  readonly onSubmit: (value: string) => void
  readonly onCancel: () => void
  readonly inputPattern?: string
  readonly inputTitle?: string
  readonly children: string
}

/** A `<span>` that can turn into an `<input type="text">`. */
export default function EditableSpan(props: EditableSpanProps) {
  const { 'data-testid': dataTestId, className, editable = false, children } = props
  const { checkSubmittable, onSubmit, onCancel, inputPattern, inputTitle } = props
  const inputBindings = inputBindingsProvider.useInputBindings()
  const [isSubmittable, setIsSubmittable] = React.useState(true)
  const inputRef = React.useRef<HTMLInputElement>(null)
  const cancelled = React.useRef(false)

  React.useEffect(() => {
    setIsSubmittable(checkSubmittable?.(inputRef.current?.value ?? '') ?? true)
    // This effect MUST only run on mount.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  React.useEffect(() => {
    if (editable) {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        cancelEditName: () => {
          onCancel()
          cancelled.current = true
          inputRef.current?.blur()
        },
      })
    } else {
      return
    }
  }, [editable, onCancel, /* should never change */ inputBindings])

  React.useEffect(() => {
    cancelled.current = false
  }, [editable])

  if (editable) {
    return (
      <form
        className="flex grow"
        onSubmit={event => {
          event.preventDefault()
          if (isSubmittable) {
            if (inputRef.current != null) {
              onSubmit(inputRef.current.value)
            }
          }
        }}
      >
        <input
          data-testid={dataTestId}
          className={className}
          ref={inputRef}
          autoFocus
          type="text"
          size={1}
          defaultValue={children}
          onBlur={event => {
            if (!cancelled.current) {
              event.currentTarget.form?.requestSubmit()
            }
          }}
          onContextMenu={event => {
            event.stopPropagation()
          }}
          onKeyDown={event => {
            if (event.key !== 'Escape') {
              // The input may handle the event.
              event.stopPropagation()
            }
          }}
          {...(inputPattern == null ? {} : { pattern: inputPattern })}
          {...(inputTitle == null ? {} : { title: inputTitle })}
          {...(checkSubmittable == null
            ? {}
            : {
                onInput: event => {
                  setIsSubmittable(checkSubmittable(event.currentTarget.value))
                },
              })}
        />
        {isSubmittable && (
          <button type="submit" className="mx-tick-cross-button">
            <img src={TickIcon} alt="Confirm Edit" />
          </button>
        )}
        <button
          type="button"
          className="mx-tick-cross-button"
          onMouseDown={() => {
            cancelled.current = true
          }}
          onClick={event => {
            event.stopPropagation()
            onCancel()
            window.setTimeout(() => {
              cancelled.current = false
            })
          }}
        >
          <img src={CrossIcon} alt="Cancel Edit" />
        </button>
      </form>
    )
  } else {
    return (
      <span data-testid={dataTestId} className={className}>
        {children}
      </span>
    )
  }
}
