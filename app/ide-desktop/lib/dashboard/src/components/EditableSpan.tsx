/** @file A text `<span>` which turns into an `input` when desired. */
import * as React from 'react'

import CrossIcon from 'enso-assets/cross.svg'
import TickIcon from 'enso-assets/tick.svg'

import * as eventCallback from '#/hooks/eventCallbackHooks'

import * as inputBindingsProvider from '#/providers/InputBindingsProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import * as eventModule from '#/utilities/event'
import * as sanitizedEventTargets from '#/utilities/sanitizedEventTargets'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

const WIDTH_CLASS_NAME = 'max-w-60'

// ====================
// === EditableSpan ===
// ====================

/** Props for an {@link EditableSpan}. */
export interface EditableSpanProps {
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
  const { className = '', editable = false, children } = props
  const { checkSubmittable, onSubmit, onCancel, inputPattern, inputTitle } = props
  const { getText } = textProvider.useText()
  const inputBindings = inputBindingsProvider.useInputBindings()
  const [isSubmittable, setIsSubmittable] = React.useState(false)
  const inputRef = React.useRef<HTMLInputElement | null>(null)
  const cancelledRef = React.useRef(false)
  const checkSubmittableRef = React.useRef(checkSubmittable)
  checkSubmittableRef.current = checkSubmittable

  // Make sure that the event callback is stable to prevent the effect from re-running.
  const onCancelEventCallback = eventCallback.useEventCallback(onCancel)

  React.useEffect(() => {
    if (editable) {
      setIsSubmittable(checkSubmittableRef.current?.(inputRef.current?.value ?? '') ?? true)
    }
  }, [editable])

  React.useEffect(() => {
    if (editable) {
      return inputBindings.attach(sanitizedEventTargets.document.body, 'keydown', {
        cancelEditName: () => {
          onCancelEventCallback()
          cancelledRef.current = true
          inputRef.current?.blur()
        },
      })
    } else {
      return
    }
  }, [editable, inputBindings, onCancelEventCallback])

  React.useEffect(() => {
    cancelledRef.current = false
  }, [editable])

  if (editable) {
    return (
      <form
        className={tailwindMerge.twMerge('flex grow gap-1.5', WIDTH_CLASS_NAME)}
        onBlur={event => {
          const currentTarget = event.currentTarget
          if (!currentTarget.contains(event.relatedTarget)) {
            // This must run AFTER the cancel button's event handler runs.
            setTimeout(() => {
              if (!cancelledRef.current) {
                currentTarget.requestSubmit()
              }
            })
          }
        }}
        onSubmit={event => {
          event.preventDefault()
          if (inputRef.current != null) {
            if (isSubmittable) {
              onSubmit(inputRef.current.value)
            } else {
              onCancel()
            }
          }
        }}
      >
        <aria.Input
          data-testid={props['data-testid']}
          className={tailwindMerge.twMerge('rounded-lg', className)}
          ref={element => {
            inputRef.current = element
            if (element) {
              element.style.width = '0'
              element.style.width = `${element.scrollWidth}px`
            }
          }}
          autoFocus
          type="text"
          size={1}
          defaultValue={children}
          onContextMenu={event => {
            event.stopPropagation()
          }}
          onKeyDown={event => {
            if (event.key !== 'Escape') {
              event.stopPropagation()
            }
            if (event.target instanceof HTMLElement) {
              event.target.style.width = '0'
              event.target.style.width = `${event.target.scrollWidth}px`
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
        <ariaComponents.ButtonGroup gap="xsmall" className="grow-0 items-center">
          {isSubmittable && (
            <ariaComponents.Button
              size="icon"
              variant="ghost"
              icon={TickIcon}
              aria-label={getText('confirmEdit')}
              onPress={eventModule.submitForm}
            />
          )}
          <ariaComponents.Button
            size="icon"
            variant="ghost"
            icon={CrossIcon}
            aria-label={getText('cancelEdit')}
            onPress={() => {
              cancelledRef.current = true
              onCancel()
              window.setTimeout(() => {
                cancelledRef.current = false
              })
            }}
          />
        </ariaComponents.ButtonGroup>
      </form>
    )
  } else {
    return (
      <ariaComponents.Text
        data-testid={props['data-testid']}
        truncate="1"
        className={tailwindMerge.twMerge(WIDTH_CLASS_NAME, className)}
      >
        {children}
      </ariaComponents.Text>
    )
  }
}
