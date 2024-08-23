/**
 * @file
 *
 * Basic input component. Input component is a component that is used to get user input in a text field.
 */
import * as React from 'react'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'

import * as mergeRefs from '#/utilities/mergeRefs'

import type { TestIdProps } from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { omit } from 'enso-common/src/utilities/data/object'
import { INPUT_STYLES } from '../variants'

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
    ariaComponents.FieldVariantProps,
    Omit<VariantProps<typeof INPUT_STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  readonly className?: string
  readonly style?: React.CSSProperties
  readonly inputRef?: React.Ref<HTMLInputElement>
  readonly addonStart?: React.ReactNode
  readonly addonEnd?: React.ReactNode
  readonly placeholder?: string
  /** The icon to display in the input. */
  readonly icon?: React.ReactElement | string | null
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
  ref: React.ForwardedRef<HTMLFieldSetElement>,
) {
  const {
    name,
    description,
    inputRef,
    addonStart,
    addonEnd,
    size,
    rounded,
    icon,
    type = 'text',
    variant,
    variants = INPUT_STYLES,
    fieldVariants,
    form,
    ...inputProps
  } = props

  const privateInputRef = React.useRef<HTMLInputElement>(null)

  const { fieldProps, formInstance } = ariaComponents.Form.useFieldRegister<
    Omit<aria.InputProps, 'children' | 'size'>,
    Schema,
    TFieldValues,
    TFieldName,
    TTransformedValues
  >({
    ...props,
    form,
    setValueAs: (value: unknown) => {
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

  const classes = variants({
    variant,
    size,
    rounded,
    invalid: fieldProps.isInvalid,
    readOnly: inputProps.readOnly,
    disabled: fieldProps.disabled || formInstance.formState.isSubmitting,
  })

  return (
    <ariaComponents.Form.Field
      {...aria.mergeProps<ariaComponents.FieldComponentProps>()(inputProps, omit(fieldProps), {
        isHidden: props.hidden,
        ref: ref,
        fullWidth: true,
        variants: fieldVariants,
        form: formInstance,
      })}
      name={props.name}
    >
      <div
        className={classes.base()}
        onClick={() => privateInputRef.current?.focus({ preventScroll: true })}
      >
        <div className={classes.content()}>
          {addonStart != null && <div className={classes.addonStart()}>{addonStart}</div>}
          {icon != null &&
            (typeof icon === 'string' ? <SvgMask src={icon} className={classes.icon()} /> : icon)}

          <div className={classes.inputContainer()}>
            <aria.Input
              {...aria.mergeProps<aria.InputProps>()(
                inputProps,
                { className: classes.textArea(), type, name },
                omit(fieldProps, 'isInvalid', 'isRequired', 'isDisabled', 'invalid'),
              )}
              ref={mergeRefs.mergeRefs(inputRef, privateInputRef, fieldProps.ref)}
            />
          </div>

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
    React.RefAttributes<HTMLInputElement>,
) => React.ReactElement
