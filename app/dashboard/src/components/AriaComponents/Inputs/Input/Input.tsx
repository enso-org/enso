/**
 * @file
 *
 * Basic input component. Input component is a component that is used to get user input in a text field.
 */
import {
  useRef,
  type CSSProperties,
  type ForwardedRef,
  type ReactElement,
  type ReactNode,
  type Ref,
} from 'react'

import type { VariantProps } from 'tailwind-variants'

import * as aria from '#/components/aria'
import {
  Form,
  Text,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type TSchema,
} from '#/components/AriaComponents'
import SvgMask from '#/components/SvgMask'
import { mergeRefs } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import type { ExtractFunction } from '#/utilities/tailwindVariants'
import { omit } from 'enso-common/src/utilities/data/object'
import { INPUT_STYLES } from '../variants'

/**
 * Props for the Input component.
 */
export interface InputProps<Schema extends TSchema, TFieldName extends FieldPath<Schema>>
  extends FieldStateProps<Omit<aria.InputProps, 'children' | 'size'>, Schema, TFieldName>,
    FieldProps,
    Omit<VariantProps<typeof INPUT_STYLES>, 'disabled' | 'invalid'> {
  readonly 'data-testid'?: string | undefined
  readonly className?: string
  readonly style?: CSSProperties
  readonly inputRef?: Ref<HTMLInputElement>
  readonly addonStart?: ReactNode
  readonly addonEnd?: ReactNode
  readonly placeholder?: string
  /** The icon to display in the input. */
  readonly icon?: ReactElement | string | null
  readonly variants?: ExtractFunction<typeof INPUT_STYLES> | undefined
  readonly fieldVariants?: FieldComponentProps<Schema>['variants']
}

/**
 * Basic input component. Input component is a component that is used to get user input in a text field.
 */
export const Input = forwardRef(function Input<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: InputProps<Schema, TFieldName>, ref: ForwardedRef<HTMLFieldSetElement>) {
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
    icon,
    type = 'text',
    variant,
    variants,
    fieldVariants,
    ...inputProps
  } = props

  const privateInputRef = useRef<HTMLInputElement>(null)

  const { fieldState, formInstance } = Form.useField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const classes = (variants ?? INPUT_STYLES)({
    variant,
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
    setValueAs: (value) => {
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
    <Form.Field
      data-testid={props['data-testid']}
      form={formInstance}
      name={name}
      fullWidth
      isHidden={inputProps.hidden}
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
      variants={fieldVariants}
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
              ref={mergeRefs(inputRef, privateInputRef, fieldRef)}
              {...aria.mergeProps<aria.InputProps>()(
                { className: classes.textArea(), type, name, min, max, isRequired, isDisabled },
                inputProps,
                omit(field, 'required', 'disabled'),
              )}
            />
          </div>

          {addonEnd != null && <div className={classes.addonEnd()}>{addonEnd}</div>}
        </div>

        {description != null && (
          <Text slot="description" className={classes.description()}>
            {description}
          </Text>
        )}
      </div>
    </Form.Field>
  )
})
