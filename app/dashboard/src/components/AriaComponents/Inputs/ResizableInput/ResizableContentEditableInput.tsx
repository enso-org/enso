/**
 * @file A resizable input that uses a content-editable div.
 */
import { useRef, type ClipboardEvent, type ForwardedRef, type HTMLAttributes } from 'react'

import type { FieldVariantProps } from '#/components/AriaComponents'
import {
  Form,
  Text,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type TSchema,
} from '#/components/AriaComponents'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { INPUT_STYLES } from '../variants'

const CONTENT_EDITABLE_STYLES = tv({
  extend: INPUT_STYLES,
  base: '',
  slots: { placeholder: 'opacity-50 absolute inset-0 pointer-events-none' },
})

/**
 * Props for a {@link ResizableContentEditableInput}.
 */
export interface ResizableContentEditableInputProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, string>,
> extends FieldStateProps<
      HTMLAttributes<HTMLDivElement> & { value: string },
      Schema,
      TFieldName,
      string
    >,
    Pick<
      VariantProps<typeof INPUT_STYLES>,
      'disabled' | 'invalid' | 'rounded' | 'size' | 'variant'
    >,
    FieldVariantProps,
    Omit<FieldProps, 'variant'>,
    FieldVariantProps,
    Pick<VariantProps<typeof INPUT_STYLES>, 'rounded' | 'size' | 'variant'>,
    Omit<
      VariantProps<typeof CONTENT_EDITABLE_STYLES>,
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
export const ResizableContentEditableInput = forwardRef(function ResizableContentEditableInput<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, string>,
>(
  props: ResizableContentEditableInputProps<Schema, TFieldName>,
  ref: ForwardedRef<HTMLDivElement>,
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

  const inputRef = useRef<HTMLDivElement>(null)

  const onPaste = useEventCallback((event: ClipboardEvent<HTMLDivElement>) => {
    // Prevent pasting styled text.
    event.preventDefault()
    // sanitize the pasted text
    // replace all < with &lt; to prevent XSS
    const text = event.clipboardData
      .getData('text/plain')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
    document.execCommand('insertHTML', false, text)
  })

  const useStringField = Form.makeUseField<string>()
  const { field, fieldState, formInstance } = useStringField({
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
    <Form.Field
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
            ref={mergeRefs(inputRef, ref, field.ref)}
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

          <Text className={placeholderClass({ class: field.value.length > 0 ? 'hidden' : '' })}>
            {placeholder}
          </Text>
        </div>

        {description != null && (
          <Text slot="description" className={descriptionClass()}>
            {description}
          </Text>
        )}
      </div>
    </Form.Field>
  )
})
