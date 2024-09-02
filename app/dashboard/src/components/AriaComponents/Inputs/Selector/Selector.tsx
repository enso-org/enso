/** @file A horizontal selector. */
import * as React from 'react'

import type * as twv from 'tailwind-variants'

import { mergeProps, type RadioGroupProps } from '#/components/aria'
import {
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  Form,
  type TSchema,
} from '#/components/AriaComponents'

import { mergeRefs } from '#/utilities/mergeRefs'

import { AnimatedBackground } from '#/components/AnimatedBackground'
import RadioGroup from '#/components/styled/RadioGroup'
import { mergeRefs } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import { tv } from '#/utilities/tailwindVariants'
import { Controller } from 'react-hook-form'
import { SelectorOption } from './SelectorOption'

/** * Props for the Selector component. */
export interface SelectorProps<Schema extends TSchema, TFieldName extends FieldPath<Schema>>
  extends FieldStateProps<
      Omit<RadioGroupProps, 'children' | 'value'> & { value: FieldValues<Schema>[TFieldName] },
      Schema,
      TFieldName
    >,
    FieldProps,
    Omit<twv.VariantProps<typeof SELECTOR_STYLES>, 'disabled' | 'invalid'> {
  readonly items: readonly FieldValues<Schema>[TFieldName][]
  readonly children?: (item: FieldValues<Schema>[TFieldName]) => string
  readonly columns?: number
  readonly className?: string
  readonly style?: React.CSSProperties
  readonly inputRef?: React.Ref<HTMLDivElement>
  readonly placeholder?: string
}

export const SELECTOR_STYLES = tv({
  base: 'block w-full bg-transparent transition-[border-color,outline] duration-200',
  variants: {
    disabled: {
      true: { base: 'cursor-default opacity-50', textArea: 'cursor-default' },
      false: { base: 'cursor-text', textArea: 'cursor-text' },
    },
    readOnly: { true: 'cursor-default' },
    size: {
      medium: { base: '' },
    },
    rounded: {
      none: 'rounded-none',
      small: 'rounded-sm',
      medium: 'rounded-md',
      large: 'rounded-lg',
      xlarge: 'rounded-xl',
      xxlarge: 'rounded-2xl',
      xxxlarge: 'rounded-3xl',
      full: 'rounded-full',
    },
    variant: {
      outline: {
        base: 'border-[0.5px] border-primary/20',
      },
    },
  },
  defaultVariants: {
    size: 'medium',
    rounded: 'xxxlarge',
    variant: 'outline',
  },
  slots: {
    radioGroup: 'flex',
  },
})

/**
 * A horizontal selector.
 */
export const Selector = forwardRef(function Selector<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema>,
>(props: SelectorProps<Schema, TFieldName>, ref: React.ForwardedRef<HTMLFieldSetElement>) {
  const {
    name,
    items,
    children = String,
    isDisabled = false,
    columns,
    form,
    defaultValue,
    inputRef,
    label,
    size,
    rounded,
    isRequired = false,
    ...inputProps
  } = props

  const privateInputRef = React.useRef<HTMLDivElement>(null)

  const { fieldState, formInstance } = Form.useField({
    name,
    isDisabled,
    form,
    ...(defaultValue != null ? { defaultValue } : {}),
  })

  const classes = SELECTOR_STYLES({
    size,
    rounded,
    readOnly: inputProps.readOnly,
    disabled: isDisabled || formInstance.formState.isSubmitting,
  })

  return (
    <Form.Field
      form={formInstance}
      name={name}
      fullWidth
      label={label}
      aria-label={props['aria-label']}
      aria-labelledby={props['aria-labelledby']}
      aria-describedby={props['aria-describedby']}
      isRequired={isRequired}
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
        <Controller
          control={formInstance.control}
          name={name}
          render={(renderProps) => {
            const { ref: fieldRef, value, onChange, ...field } = renderProps.field
            return (
              <RadioGroup
                ref={mergeRefs(inputRef, privateInputRef, fieldRef)}
                {...mergeProps<RadioGroupProps>()(
                  {
                    className: classes.radioGroup(),
                    name,
                    isRequired,
                    isDisabled,
                    style:
                      columns != null ? { gridTemplateColumns: `repeat(${columns}, 1fr)` } : {},
                  },
                  inputProps,
                  field,
                )}
                // eslint-disable-next-line no-restricted-syntax
                aria-label={props['aria-label'] ?? (typeof label === 'string' ? label : '')}
                value={String(items.indexOf(value))}
                onChange={(newValue) => {
                  // eslint-disable-next-line @typescript-eslint/no-unsafe-return
                  onChange(items[Number(newValue)])
                }}
              >
                <AnimatedBackground value={String(items.indexOf(value))}>
                  {items.map((item, i) => (
                    <SelectorOption key={i} value={String(i)} label={children(item)} />
                  ))}
                </AnimatedBackground>
              </RadioGroup>
            )
          }}
        />
      </div>
    </Form.Field>
  )
})
