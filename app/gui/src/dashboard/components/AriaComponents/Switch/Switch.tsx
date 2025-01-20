/**
 * @file
 *
 * A switch allows a user to turn a setting on or off.
 */
import { useRef, type CSSProperties, type ForwardedRef } from 'react'

import {
  Switch as AriaSwitch,
  mergeProps,
  type SwitchProps as AriaSwitchProps,
} from '#/components/aria'
import { mergeRefs } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { Form, type FieldPath, type FieldProps, type FieldStateProps, type TSchema } from '../Form'
import { TEXT_STYLE } from '../Text'

/** Props for the {@Switch} component. */
export interface SwitchProps<Schema extends TSchema, TFieldName extends FieldPath<Schema>>
  extends FieldStateProps<
      Omit<AriaSwitchProps, 'children' | 'size' | 'value'> & { value: boolean },
      Schema,
      TFieldName
    >,
    FieldProps,
    Omit<VariantProps<typeof SWITCH_STYLES>, 'disabled' | 'invalid'> {
  readonly className?: string
  readonly style?: CSSProperties
}

export const SWITCH_STYLES = tv({
  base: '',
  variants: {
    disabled: { true: 'cursor-not-allowed opacity-50' },
    size: {
      small: {
        background: 'h-4 w-7 p-0.5',
      },
    },
  },
  slots: {
    switch: 'group flex items-center gap-1',
    label: TEXT_STYLE({
      variant: 'body',
      color: 'primary',
      className: 'flex-1',
    }),
    background:
      'flex shrink-0 cursor-default items-center rounded-full bg-primary/30 bg-clip-padding shadow-inner outline-none ring-black transition duration-200 ease-in-out group-focus-visible:ring-2 group-pressed:bg-primary/60 group-selected:bg-primary group-selected:group-pressed:bg-primary/50',
    thumb:
      'aspect-square h-full flex-none translate-x-0 transform rounded-full bg-white transition duration-200 ease-in-out group-selected:translate-x-[100%]',
  },
  defaultVariants: {
    size: 'small',
    disabled: false,
  },
})

/** A switch allows a user to turn a setting on or off. */
export const Switch = forwardRef(function Switch<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: SwitchProps<Schema, TFieldName>, ref: ForwardedRef<HTMLFieldSetElement>) {
  const {
    label,
    isDisabled = false,
    isRequired = false,
    defaultValue,
    className,
    name,
    form,
    description,
    error,
    size,
    ...ariaSwitchProps
  } = props

  const switchRef = useRef<HTMLInputElement>(null)

  const { fieldState, formInstance, field } = Form.useField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const { ref: fieldRef, ...fieldProps } = formInstance.register(name, {
    disabled: isDisabled,
    required: isRequired,
    ...(props.onBlur && { onBlur: props.onBlur }),
    ...(props.onChange && {
      onChange: (event: { target: { value: boolean } }) => props.onChange?.(event.target.value),
    }),
  })

  const {
    base,
    thumb,
    background,
    label: labelStyle,
    switch: switchStyles,
  } = SWITCH_STYLES({ size, disabled: fieldProps.disabled })

  return (
    <Form.Field
      ref={ref}
      form={formInstance}
      name={name}
      className={base({ className })}
      fullWidth
      description={description}
      error={error}
      aria-label={props['aria-label']}
      aria-labelledby={props['aria-labelledby']}
      aria-describedby={props['aria-describedby']}
      isRequired={fieldProps.required}
      isInvalid={fieldState.invalid}
      aria-details={props['aria-details']}
      style={props.style}
    >
      <AriaSwitch
        ref={(el) => {
          mergeRefs(switchRef, fieldRef)(el)
        }}
        {...mergeProps<AriaSwitchProps>()(ariaSwitchProps, fieldProps, {
          defaultSelected: field.value,
          className: switchStyles(),
          onChange: field.onChange,
          onBlur: field.onBlur,
        })}
      >
        <div className={background()} role="presentation">
          <span className={thumb()} />
        </div>

        <div className={labelStyle()}>{label}</div>
      </AriaSwitch>
    </Form.Field>
  )
})
