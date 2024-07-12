/**
 * @file
 *
 * Basic input component. Input component is a component that is used to get user input in a text field.
 */
import * as React from 'react'

import type * as twv from 'tailwind-variants'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import * as mergeRefs from '#/utilities/mergeRefs'

import * as variants from '../variants'

/**
 * Props for the Input component.
 */
export interface InputProps<
  Schema extends ariaComponents.TSchema,
  TFieldValues extends ariaComponents.FieldValues<Schema>,
  TFieldName extends ariaComponents.FieldPath<Schema, TFieldValues>,
  TTransformedValues extends ariaComponents.FieldValues<Schema> | undefined = undefined,
> extends ariaComponents.FieldStateProps<
      Omit<aria.InputProps, 'children' | 'size'>,
      Schema,
      TFieldValues,
      TFieldName,
      TTransformedValues
    >,
    ariaComponents.FieldProps,
    Omit<twv.VariantProps<typeof variants.INPUT_STYLES>, 'disabled' | 'invalid'> {
  readonly className?: string
  readonly style?: React.CSSProperties
  readonly inputRef?: React.Ref<HTMLInputElement>
  readonly addonStart?: React.ReactNode
  readonly addonEnd?: React.ReactNode
  readonly placeholder?: string
}

/**
 * Basic input component. Input component is a component that is used to get user input in a text field.
 */
// eslint-disable-next-line no-restricted-syntax
export const Input = React.forwardRef(function Input<
  Schema extends ariaComponents.TSchema,
  TFieldValues extends ariaComponents.FieldValues<Schema>,
  TFieldName extends ariaComponents.FieldPath<Schema, TFieldValues>,
  TTransformedValues extends ariaComponents.FieldValues<Schema> | undefined = undefined,
>(
  props: InputProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
  ref: React.ForwardedRef<HTMLFieldSetElement>
) {
  const {
    name,
    isDisabled = false,
    form,
    defaultValue,
    description,
    inputRef,
    addonStart,
    addonEnd,
    label,
    size,
    rounded,
    isRequired = false,
    min,
    max,
    type = 'text',
    ...inputProps
  } = props

  const privateInputRef = React.useRef<HTMLInputElement>(null)

  const { fieldState, formInstance } = ariaComponents.Form.useField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const classes = variants.INPUT_STYLES({
    size,
    rounded,
    invalid: fieldState.invalid,
    readOnly: inputProps.readOnly,
    disabled: isDisabled || formInstance.formState.isSubmitting,
  })

  const { ref: fieldRef, ...field } = formInstance.register(name, {
    disabled: isDisabled,
    required: isRequired,
    ...(inputProps.onBlur && { onBlur: inputProps.onBlur }),
    ...(inputProps.onChange && { onChange: inputProps.onChange }),
    ...(inputProps.minLength != null ? { minLength: inputProps.minLength } : {}),
    ...(inputProps.maxLength != null ? { maxLength: inputProps.maxLength } : {}),
    ...(min != null ? { min } : {}),
    ...(max != null ? { max } : {}),
    setValueAs: value => {
      if (typeof value === 'string') {
        if (type === 'number') {
          return Number(value)
        } else if (type === 'date') {
          return new Date(value)
        } else {
          return value
        }
      } else {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-return
        return value
      }
    },
  })

  return (
    <ariaComponents.Form.Field
      form={formInstance}
      name={name}
      fullWidth
      label={label}
      aria-label={props['aria-label']}
      aria-labelledby={props['aria-labelledby']}
      aria-describedby={props['aria-describedby']}
      isRequired={field.required}
      isInvalid={fieldState.invalid}
      aria-details={props['aria-details']}
      ref={ref}
      style={props.style}
      className={props.className}
    >
      <div
        className={classes.base()}
        onClick={() => privateInputRef.current?.focus({ preventScroll: true })}
      >
        <div className={classes.inputContainer()}>
          {addonStart != null && <div className={classes.addonStart()}>{addonStart}</div>}

          <aria.Input
            ref={mergeRefs.mergeRefs(inputRef, privateInputRef, fieldRef)}
            {...aria.mergeProps<aria.InputProps>()(
              { className: classes.textArea(), type, name, min, max, isRequired, isDisabled },
              inputProps,
              field
            )}
          />

          {addonEnd != null && <div className={classes.addonEnd()}>{addonEnd}</div>}
        </div>

        {description != null && (
          <ariaComponents.Text slot="description" className={classes.description()}>
            {description}
          </ariaComponents.Text>
        )}
      </div>
    </ariaComponents.Form.Field>
  )
}) as <
  Schema extends ariaComponents.TSchema,
  TFieldValues extends ariaComponents.FieldValues<Schema>,
  TFieldName extends ariaComponents.FieldPath<Schema, TFieldValues>,
  TTransformedValues extends ariaComponents.FieldValues<Schema> | undefined = undefined,
>(
  props: InputProps<Schema, TFieldValues, TFieldName, TTransformedValues> &
    React.RefAttributes<HTMLInputElement>
) => React.ReactElement
