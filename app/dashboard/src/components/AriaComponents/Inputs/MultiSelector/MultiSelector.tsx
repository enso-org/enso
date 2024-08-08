/** @file A horizontal selector. */
import * as React from 'react'

import type * as twv from 'tailwind-variants'

import { ListBox, mergeProps, type ListBoxItemProps, type ListBoxProps } from '#/components/aria'
import {
  Form,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type TSchema,
} from '#/components/AriaComponents'

import { mergeRefs } from '#/utilities/mergeRefs'

import { tv } from '#/utilities/tailwindVariants'
import { Controller } from 'react-hook-form'
import { MultiSelectorOption } from './MultiSelectorOption'

/** * Props for the MultiSelector component. */
export interface MultiSelectorProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends FieldStateProps<
      Omit<ListBoxItemProps, 'children' | 'value'> & { value: TFieldValues[TFieldName] },
      Schema,
      TFieldValues,
      TFieldName,
      TTransformedValues
    >,
    FieldProps,
    Omit<twv.VariantProps<typeof MULTI_SELECTOR_STYLES>, 'disabled' | 'invalid'> {
  readonly items: readonly Extract<TFieldValues[TFieldName], readonly unknown[]>[number][]
  readonly itemToString?: (
    item: Extract<TFieldValues[TFieldName], readonly unknown[]>[number],
  ) => string
  readonly columns?: number
  readonly className?: string
  readonly style?: React.CSSProperties
  readonly inputRef?: React.Ref<HTMLDivElement>
  readonly placeholder?: string
}

export const MULTI_SELECTOR_STYLES = tv({
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
    listBox: 'grid',
  },
})

/**
 * A horizontal multi-selector.
 */
// eslint-disable-next-line no-restricted-syntax
export const MultiSelector = React.forwardRef(function MultiSelector<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  props: MultiSelectorProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
  ref: React.ForwardedRef<HTMLFieldSetElement>,
) {
  const {
    name,
    items,
    itemToString = String,
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
    defaultValue,
  })

  const classes = MULTI_SELECTOR_STYLES({
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
            const { ref: fieldRef, onChange, ...field } = renderProps.field
            return (
              <ListBox
                ref={mergeRefs(inputRef, privateInputRef, fieldRef)}
                orientation="horizontal"
                selectionMode="multiple"
                {...mergeProps<ListBoxProps<TFieldValues[TFieldName]>>()(
                  {
                    className: classes.listBox(),
                    style: { gridTemplateColumns: `repeat(${columns ?? items.length}, 1fr)` },
                  },
                  // @ts-expect-error This is UNSAFE. This error is caused by type mismatches for
                  // the `id` and `aria-*` properties.
                  inputProps,
                  field,
                )}
                // eslint-disable-next-line no-restricted-syntax
                aria-label={props['aria-label'] ?? (typeof label === 'string' ? label : '')}
                onSelectionChange={(selection) => {
                  // eslint-disable-next-line @typescript-eslint/no-unsafe-return
                  onChange([...selection].map((key) => items[Number(key)]))
                }}
              >
                {items.map((item, i) => (
                  <MultiSelectorOption key={i} id={i} value={{ item }} label={itemToString(item)} />
                ))}
              </ListBox>
            )
          }}
        />
      </div>
    </Form.Field>
  )
}) as unknown as <
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  props: MultiSelectorProps<Schema, TFieldValues, TFieldName, TTransformedValues> &
    React.RefAttributes<HTMLDivElement>,
) => React.ReactElement
