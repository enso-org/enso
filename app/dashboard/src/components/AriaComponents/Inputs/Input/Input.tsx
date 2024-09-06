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

import { Controller } from 'react-hook-form'
import type { VariantProps } from 'tailwind-variants'

import * as aria from '#/components/aria'
import {
  Form,
  Text,
  type FieldComponentProps,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FormInstance,
  type TSchema,
} from '#/components/AriaComponents'
import { useFormContext } from '#/components/AriaComponents/Form/components/useFormContext'
import SvgMask from '#/components/SvgMask'
import { mergeRefs } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import type { ExtractFunction } from '#/utilities/tailwindVariants'
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
    form: formRaw,
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
  const formFromContext = useFormContext()
  // eslint-disable-next-line no-restricted-syntax
  const form = (formRaw ?? formFromContext) as FormInstance<TSchema>

  const privateInputRef = useRef<HTMLInputElement>(null)

  return (
    <Controller
      control={form.control}
      name={name}
      disabled={isDisabled}
      // eslint-disable-next-line no-restricted-syntax
      defaultValue={defaultValue as never}
      rules={{
        required: isRequired,
        ...(inputProps.onBlur && { onBlur: inputProps.onBlur }),
        ...(inputProps.onChange && { onChange: inputProps.onChange }),
        ...(inputProps.minLength != null ? { minLength: inputProps.minLength } : {}),
        ...(inputProps.maxLength != null ? { maxLength: inputProps.maxLength } : {}),
        ...(min != null ? { min } : {}),
        ...(max != null ? { max } : {}),
      }}
      render={({ field, fieldState: innerFieldState }) => {
        const classes = (variants ?? INPUT_STYLES)({
          variant,
          size,
          rounded,
          invalid: innerFieldState.invalid,
          readOnly: inputProps.readOnly,
          disabled: isDisabled || form.formState.isSubmitting,
        })
        return (
          <Form.Field
            data-testid={props['data-testid']}
            form={form}
            name={name}
            fullWidth
            isHidden={inputProps.hidden}
            label={label}
            aria-label={props['aria-label']}
            aria-labelledby={props['aria-labelledby']}
            aria-describedby={props['aria-describedby']}
            isRequired={isRequired}
            isInvalid={innerFieldState.invalid}
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
                  (typeof icon === 'string' ?
                    <SvgMask src={icon} className={classes.icon()} />
                  : icon)}

                <div className={classes.inputContainer()}>
                  <aria.Input
                    {...aria.mergeProps<aria.InputProps>()(
                      { className: classes.textArea(), type, name, min, max },
                      inputProps,
                      field,
                    )}
                    ref={mergeRefs(inputRef, privateInputRef, field.ref)}
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
      }}
    />
  )
})
