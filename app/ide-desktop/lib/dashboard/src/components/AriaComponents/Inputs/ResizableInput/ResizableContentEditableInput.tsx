/**
 * @file A resizable input that uses a content-editable div.
 */
import * as React from 'react'

import * as twv from 'tailwind-variants'

import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

import * as aria from '#/components/aria'

import * as mergeRefs from '#/utilities/mergeRefs'

import * as varants from './variants'

const CONTENT_EDITABLE_STYLES = twv.tv({
  extend: varants.INPUT_STYLES,
  base: '',
  slots: { placeholder: 'text-primary/25 absolute inset-0 pointer-events-none' },
})

/**
 * Props for a {@link ResizableContentEditableInput}.
 */
export interface ResizableContentEditableInputProps extends aria.TextFieldProps {
  /**
   * onChange is called when the content of the input changes.
   * There is no way to prevent the change, so the value is always the new value.
   * This is different from the onChange event of a normal input element.
   * So the component is not a ***fully*** controlled component.
   */
  // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
  readonly onChange?: (value: string) => void
  readonly placeholder?: string
  readonly description?: React.ReactNode
  readonly errorMessage?: string | null
}

/**
 * A resizable input that uses a content-editable div.
 * This component might be useful for a text input that needs to have highlighted content inside of it.
 */
export const ResizableContentEditableInput = React.forwardRef(
  function ResizableContentEditableInput(
    props: ResizableContentEditableInputProps,
    ref: React.ForwardedRef<HTMLDivElement>
  ) {
    const {
      value = '',
      placeholder = '',
      onChange,
      description = null,
      errorMessage,
      onBlur,
      ...textFieldProps
    } = props

    const inputRef = React.useRef<HTMLDivElement>(null)

    const onPaste = eventCallbackHooks.useEventCallback(
      (event: React.ClipboardEvent<HTMLDivElement>) => {
        // Prevent pasting styled text.
        event.preventDefault()
        // sanitize the pasted text
        // replace all < with &lt; to prevent XSS
        const text = event.clipboardData
          .getData('text/plain')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;')
        document.execCommand('insertHTML', false, text)
      }
    )

    const {
      base,
      description: descriptionClass,
      inputContainer,
      error,
      textArea,
      placeholder: placeholderClass,
    } = CONTENT_EDITABLE_STYLES({ isInvalid: textFieldProps.isInvalid })

    return (
      <aria.TextField {...textFieldProps}>
        <div
          className={base()}
          onClick={() => {
            inputRef.current?.focus({ preventScroll: true })
          }}
        >
          <div className={inputContainer()}>
            <div
              className={textArea()}
              ref={mergeRefs.mergeRefs(inputRef, ref)}
              contentEditable
              suppressContentEditableWarning
              role="textbox"
              autoCorrect="off"
              autoCapitalize="off"
              spellCheck="false"
              aria-autocomplete="none"
              onPaste={onPaste}
              onBlur={onBlur}
              onInput={event => {
                onChange?.(event.currentTarget.textContent ?? '')
              }}
            />

            <span className={placeholderClass({ class: value ? 'hidden' : '' })}>
              {placeholder}
            </span>
          </div>

          {description != null && (
            <aria.Text slot="description" className={descriptionClass()}>
              {description}
            </aria.Text>
          )}
        </div>

        {errorMessage != null && (
          <aria.Text slot="errorMessage" className={error()}>
            {errorMessage}
          </aria.Text>
        )}
      </aria.TextField>
    )
  }
)
