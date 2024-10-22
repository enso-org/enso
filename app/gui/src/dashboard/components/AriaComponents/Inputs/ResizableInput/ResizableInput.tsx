/** @file A resizable input field. */
import * as React from 'react'

import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

import * as aria from '#/components/aria'

import * as mergeRefs from '#/utilities/mergeRefs'

import { forwardRef } from '#/utilities/react'
import * as variants from '../variants'

/** Props for a {@link ResizableInput}. */
export interface ResizableInputProps extends aria.TextFieldProps {
  readonly placeholder?: string
  readonly description?: React.ReactNode
}

/** A resizable input field. */
export const ResizableInput = forwardRef(function ResizableInput(
  props: ResizableInputProps,
  ref: React.ForwardedRef<HTMLTextAreaElement>,
) {
  const { value = '', placeholder = '', description = null, ...textFieldProps } = props
  const inputRef = React.useRef<HTMLTextAreaElement>(null)
  const resizableAreaRef = React.useRef<HTMLSpanElement>(null)

  const onPaste = eventCallbackHooks.useEventCallback(
    (event: React.ClipboardEvent<HTMLTextAreaElement>) => {
      // Prevent pasting styled text.
      event.preventDefault()
      const text = event.clipboardData.getData('text/plain')
      document.execCommand('insertHTML', false, text)
    },
  )

  React.useLayoutEffect(() => {
    if (inputRef.current && resizableAreaRef.current) {
      inputRef.current.style.height = resizableAreaRef.current.clientHeight + 'px'
    }
  }, [value])

  const {
    base,
    description: descriptionClass,
    inputContainer,
    resizableSpan,
    textArea,
  } = variants.INPUT_STYLES({ invalid: textFieldProps.isInvalid })

  return (
    <aria.TextField {...textFieldProps}>
      <div
        className={base()}
        onClick={(event) => {
          if (event.target !== inputRef.current && inputRef.current) {
            inputRef.current.focus({ preventScroll: true })
          }
        }}
      >
        <div className={inputContainer()}>
          <aria.TextArea
            ref={mergeRefs.mergeRefs(inputRef, ref)}
            onPaste={onPaste}
            className={textArea()}
            placeholder={placeholder}
          />

          <span ref={resizableAreaRef} className={resizableSpan()}>
            {value || placeholder}
          </span>
        </div>

        {description != null && (
          <aria.Text slot="description" className={descriptionClass()}>
            {description}
          </aria.Text>
        )}
      </div>
    </aria.TextField>
  )
})
