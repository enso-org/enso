/**
 * @file A resizable input that uses a content-editable div.
 */
import * as React from 'react'

import * as eventCallbackHooks from '#/hooks/eventCallbackHooks'

import * as ariaComponents from '#/components/AriaComponents'

import * as mergeRefs from '#/utilities/mergeRefs'
import * as twv from '#/utilities/tailwindVariants'

import { INPUT_STYLES } from '../variants'

const CONTENT_EDITABLE_STYLES = twv.tv({
  extend: INPUT_STYLES,
  base: '',
  slots: { placeholder: 'opacity-50 absolute inset-0 pointer-events-none' },
})

/**
 * Props for a {@link ResizableContentEditableInput}.
 */
export interface ResizableContentEditableInputProps<
  Schema extends ariaComponents.TSchema,
  TFieldValues extends ariaComponents.FieldValues<Schema>,
  TFieldName extends ariaComponents.FieldPath<Schema, TFieldValues>,
  TTransformedValues extends ariaComponents.FieldValues<Schema> | undefined = undefined,
> extends ariaComponents.FieldStateProps<
      React.HTMLAttributes<HTMLDivElement> & { value: string },
      Schema,
      TFieldValues,
      TFieldName,
      TTransformedValues
    >,
    ariaComponents.FieldVariantProps,
    Omit<ariaComponents.FieldProps, 'variant'>,
    Pick<twv.VariantProps<typeof INPUT_STYLES>, 'rounded' | 'size' | 'variant'>,
    Omit<
      twv.VariantProps<typeof CONTENT_EDITABLE_STYLES>,
      'disabled' | 'invalid' | 'rounded' | 'size' | 'variant'
    > {
  /**
   * onChange is called when the content of the input changes.
   * There is no way to prevent the change, so the value is always the new value.
   * This is different from the onChange event of a normal input element.
   * So the component is not a ***fully*** controlled component.
   */
  // eslint-disable-next-line @typescript-eslint/no-invalid-void-type
  readonly placeholder?: string
}

/**
 * A resizable input that uses a content-editable div.
 * This component might be useful for a text input that needs to have highlighted content inside of it.
 */
// eslint-disable-next-line no-restricted-syntax
export const ResizableContentEditableInput = React.forwardRef(
  function ResizableContentEditableInput<
    Schema extends ariaComponents.TSchema,
    TFieldName extends ariaComponents.FieldPath<Schema, TFieldValues>,
    TFieldValues extends ariaComponents.FieldValues<Schema> = ariaComponents.FieldValues<Schema>,
    // eslint-disable-next-line no-restricted-syntax
    TTransformedValues extends ariaComponents.FieldValues<Schema> | undefined = undefined,
  >(
    props: ResizableContentEditableInputProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
    ref: React.ForwardedRef<HTMLDivElement>,
  ) {
    const {
      placeholder = '',
      description = null,
      name,
      isDisabled = false,
      form,
      defaultValue,
      size,
      rounded,
      variant,
      variants = CONTENT_EDITABLE_STYLES,
      fieldVariants,
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
      },
    )

    const { field, fieldState, formInstance } = ariaComponents.Form.useField({
      name,
      isDisabled,
      form,
      defaultValue,
    })

    const {
      base,
      description: descriptionClass,
      inputContainer,
      textArea,
      placeholder: placeholderClass,
    } = variants({
      invalid: fieldState.invalid,
      disabled: isDisabled || formInstance.formState.isSubmitting,
      variant,
      rounded,
      size,
    })

    return (
      <ariaComponents.Form.Field
        form={formInstance}
        name={name}
        fullWidth
        variants={fieldVariants}
        {...textFieldProps}
      >
        <div
          className={base()}
          onClick={() => {
            inputRef.current?.focus({ preventScroll: true })
          }}
        >
          <div className={inputContainer()}>
            <div
              className={textArea()}
              ref={mergeRefs.mergeRefs(inputRef, ref, field.ref)}
              contentEditable
              suppressContentEditableWarning
              role="textbox"
              autoCorrect="off"
              autoCapitalize="off"
              spellCheck="false"
              aria-autocomplete="none"
              onPaste={onPaste}
              onBlur={field.onBlur}
              onInput={(event) => {
                field.onChange(event.currentTarget.textContent ?? '')
              }}
            />

            <ariaComponents.Text
              className={placeholderClass({ class: field.value.length > 0 ? 'hidden' : '' })}
            >
              {placeholder}
            </ariaComponents.Text>
          </div>

          {description != null && (
            <ariaComponents.Text slot="description" className={descriptionClass()}>
              {description}
            </ariaComponents.Text>
          )}
        </div>
      </ariaComponents.Form.Field>
    )
  },
) as <
  Schema extends ariaComponents.TSchema,
  TFieldName extends ariaComponents.FieldPath<Schema, TFieldValues>,
  TFieldValues extends ariaComponents.FieldValues<Schema> = ariaComponents.FieldValues<Schema>,
  // eslint-disable-next-line no-restricted-syntax
  TTransformedValues extends ariaComponents.FieldValues<Schema> | undefined = undefined,
>(
  props: React.RefAttributes<HTMLDivElement> &
    ResizableContentEditableInputProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
) => React.JSX.Element
