/** @file A horizontal selector. */
import * as React from 'react'

import { mergeProps, type RadioGroupProps } from '#/components/aria'
import type { FieldComponentProps } from '#/components/AriaComponents'
import {
  Form,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type FieldVariantProps,
  type TSchema,
} from '#/components/AriaComponents'

import { AnimatedBackground } from '#/components/AnimatedBackground'
import RadioGroup from '#/components/styled/RadioGroup'
import { mergeRefs } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { SelectorOption } from './SelectorOption'

/** * Props for the Selector component. */
export interface SelectorProps<Schema extends TSchema, TFieldName extends FieldPath<Schema>>
  extends FieldStateProps<
      Omit<RadioGroupProps, 'children' | 'value'> & { value: FieldValues<Schema>[TFieldName] },
      Schema,
      TFieldName
    >,
    FieldProps,
    Omit<VariantProps<typeof SELECTOR_STYLES>, 'disabled' | 'invalid' | 'variants'>,
    FieldVariantProps {
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

/** A horizontal selector. */
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
    inputRef,
    label,
    size,
    rounded,
    isRequired = false,
    isInvalid = false,
    fieldVariants,
    defaultValue,
    ...inputProps
  } = props

  const privateInputRef = React.useRef<HTMLDivElement>(null)

  const formInstance = Form.useFormContext(form)

  const classes = SELECTOR_STYLES({
    size,
    rounded,
    readOnly: inputProps.readOnly,
    disabled: isDisabled || formInstance.formState.isSubmitting,
  })

  return (
    <Form.Controller
      control={formInstance.control}
      name={name}
      render={(renderProps) => {
        const { value } = renderProps.field
        return (
          <Form.Field
            {...mergeProps<FieldComponentProps<Schema>>()(inputProps, renderProps.field, {
              fullWidth: true,
              variants: fieldVariants,
              form: formInstance,
              label,
              isRequired,
            })}
            name={props.name}
            ref={ref}
          >
            <div
              className={classes.base()}
              onClick={() => privateInputRef.current?.focus({ preventScroll: true })}
            >
              <RadioGroup
                {...mergeProps<RadioGroupProps>()(
                  {
                    className: classes.radioGroup(),
                    name,
                    isRequired,
                    isDisabled,
                    isInvalid,
                    style:
                      columns != null ? { gridTemplateColumns: `repeat(${columns}, 1fr)` } : {},
                    ...(defaultValue != null ? { defaultValue } : {}),
                  },
                  inputProps,
                  renderProps.field,
                )}
                ref={mergeRefs(inputRef, privateInputRef, renderProps.field.ref)}
                // eslint-disable-next-line no-restricted-syntax
                aria-label={props['aria-label'] ?? (typeof label === 'string' ? label : '')}
                value={String(items.indexOf(value))}
                onChange={(newValue) => {
                  renderProps.field.onChange(items[Number(newValue)])
                }}
              >
                <AnimatedBackground value={String(items.indexOf(value))}>
                  {items.map((item, i) => (
                    <SelectorOption key={i} value={String(i)} label={children(item)} />
                  ))}
                </AnimatedBackground>
              </RadioGroup>
            </div>
          </Form.Field>
        )
      }}
    />
  )
})
