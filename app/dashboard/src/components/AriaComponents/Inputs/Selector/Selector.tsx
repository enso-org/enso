/** @file A horizontal selector. */
import * as React from 'react'

import type * as twv from 'tailwind-variants'

import { mergeProps, type RadioGroupProps } from '#/components/aria'
import {
  FieldComponentProps,
  Form,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type FieldVariantProps,
  type TSchema,
} from '#/components/AriaComponents'

import { mergeRefs } from '#/utilities/mergeRefs'

import { AnimatedBackground } from '#/components/AnimatedBackground'
import RadioGroup from '#/components/styled/RadioGroup'
import { tv } from '#/utilities/tailwindVariants'
import { omit } from 'enso-common/src/utilities/data/object'
import { Controller } from 'react-hook-form'
import { SelectorOption } from './SelectorOption'

/** * Props for the Selector component. */
export interface SelectorProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends FieldStateProps<
      Omit<RadioGroupProps, 'children' | 'value'> & { value: TFieldValues[TFieldName] },
      Schema,
      TFieldValues,
      TFieldName,
      TTransformedValues
    >,
    FieldProps,
    Omit<twv.VariantProps<typeof SELECTOR_STYLES>, 'disabled' | 'invalid'>,
    FieldVariantProps {
  readonly items: readonly TFieldValues[TFieldName][]
  readonly itemToString?: (item: TFieldValues[TFieldName]) => string
  readonly className?: string
  readonly style?: React.CSSProperties
  readonly inputRef?: React.Ref<HTMLDivElement>
  readonly placeholder?: string
  readonly readOnly?: boolean
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
// eslint-disable-next-line no-restricted-syntax
export const Selector = React.forwardRef(function Selector<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  props: SelectorProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
  ref: React.ForwardedRef<HTMLFieldSetElement>,
) {
  const {
    name,
    items,
    itemToString = String,
    isDisabled,
    form,
    defaultValue,
    inputRef,
    label,
    size,
    rounded,
    isRequired = false,
    fieldVariants,
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
    <Controller
      control={formInstance.control}
      name={name}
      render={(renderProps) => {
        const { value } = renderProps.field
        return (
          <Form.Field
            {...mergeProps<FieldComponentProps>()(inputProps, renderProps.field, {
              ref,
              fullWidth: true,
              variants: fieldVariants,
              form: formInstance,
              label,
              isRequired,
            })}
            name={props.name}
          >
            <div
              className={classes.base()}
              onClick={() => privateInputRef.current?.focus({ preventScroll: true })}
            >
              <RadioGroup
                {...mergeProps<RadioGroupProps>()(
                  { className: classes.radioGroup(), name },
                  omit(inputProps, 'isInvalid'),
                  renderProps.field,
                )}
                ref={mergeRefs(inputRef, privateInputRef, renderProps.field.ref)}
                // eslint-disable-next-line no-restricted-syntax
                aria-label={props['aria-label'] ?? (typeof label === 'string' ? label : '')}
                value={String(items.indexOf(value))}
                onChange={(newValue) => {
                  // eslint-disable-next-line @typescript-eslint/no-unsafe-return
                  renderProps.field.onChange(items[Number(newValue)])
                }}
              >
                <AnimatedBackground value={String(items.indexOf(value))}>
                  {items.map((item, i) => (
                    <SelectorOption key={i} value={String(i)} label={itemToString(item)} />
                  ))}
                </AnimatedBackground>
              </RadioGroup>
            </div>
          </Form.Field>
        )
      }}
    />
  )
}) as <
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  props: React.RefAttributes<HTMLDivElement> &
    SelectorProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
) => React.ReactElement
