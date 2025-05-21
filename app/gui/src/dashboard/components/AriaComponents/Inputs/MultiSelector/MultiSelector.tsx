/** @file A horizontal selector supporting multiple input. */
import { forwardRef, useRef, type CSSProperties, type ForwardedRef, type Ref } from 'react'

import { omit, unsafeRemoveUndefined } from 'enso-common/src/utilities/data/object'

import {
  FieldError,
  ListBox,
  mergeProps,
  type ListBoxItemProps,
  type ListBoxProps,
} from '#/components/aria'
import {
  Form,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type TSchema,
} from '#/components/AriaComponents/Form'
import { MULTI_SELECTOR_STYLES } from '#/components/AriaComponents/Inputs/MultiSelector/variants'
import { mergeRefs } from '#/utilities/mergeRefs'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { MultiSelectorOption, type MultiSelectorOptionProps } from './MultiSelectorOption'

const OPTION_VARIANTS: Record<
  MultiSelectorProps<never, never, never>['variant'] & {},
  MultiSelectorOptionProps['variant'] & {}
> = {
  outline: 'default',
  // eslint-disable-next-line @typescript-eslint/naming-convention
  'separate-outline': 'outline',
}

/** * Props for the MultiSelector component. */
export interface MultiSelectorProps<
  Schema extends TSchema,
  FieldName extends FieldPath<Schema, readonly T[]>,
  T,
> extends FieldStateProps<
      Omit<ListBoxItemProps, 'children' | 'value'> & { value: FieldValues<Schema>[FieldName] },
      Schema,
      FieldName,
      readonly T[]
    >,
    FieldProps,
    Omit<VariantProps<typeof MULTI_SELECTOR_STYLES>, 'disabled' | 'invalid'> {
  readonly items: readonly T[]
  readonly children?: (item: T) => string
  readonly columns?: number
  readonly className?: string
  readonly style?: CSSProperties
  readonly inputRef?: Ref<HTMLDivElement>
  readonly placeholder?: string
}

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-explicit-any
const useReadonlyArrayField = Form.makeUseField<readonly any[]>()

/** A horizontal multi-selector. */
export const MultiSelector = forwardRef(function MultiSelector<
  Schema extends TSchema,
  FieldName extends FieldPath<Schema, readonly T[]>,
  T,
>(props: MultiSelectorProps<Schema, FieldName, T>, ref: ForwardedRef<HTMLDivElement>) {
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
    variant,
    contextualHelp,
    ...inputProps
  } = props

  const privateInputRef = useRef<HTMLDivElement>(null)

  // eslint-disable-next-line no-restricted-syntax
  const { fieldState, formInstance } = useReadonlyArrayField({
    name,
    isDisabled,
    form,
    defaultValue,
  }) as unknown as ReturnType<ReturnType<typeof Form.makeUseField<readonly T[]>>>

  const classes = MULTI_SELECTOR_STYLES({
    size,
    rounded,
    readOnly: inputProps.readOnly,
    disabled: isDisabled || formInstance.formState.isSubmitting,
    variant,
  })

  const optionVariant = OPTION_VARIANTS[variant ?? 'outline']

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
      contextualHelp={contextualHelp}
    >
      <div
        className={classes.base()}
        onClick={() => privateInputRef.current?.focus({ preventScroll: true })}
      >
        <Form.Controller
          control={formInstance.control}
          name={name}
          render={(renderProps) => {
            const { ref: fieldRef, value, onChange, ...field } = renderProps.field
            return (
              <ListBox
                ref={mergeRefs(inputRef, privateInputRef, fieldRef)}
                orientation="horizontal"
                selectionMode="multiple"
                {...(inputProps.id != null && { id: String(inputProps.id) })}
                {...mergeProps<ListBoxProps<FieldValues<Schema>[FieldName]>>()(
                  {
                    className: classes.listBox(),
                    style: { gridTemplateColumns: `repeat(${columns ?? items.length}, 1fr)` },
                  },
                  unsafeRemoveUndefined(omit(inputProps, 'id')),
                  field,
                )}
                // eslint-disable-next-line no-restricted-syntax
                aria-label={props['aria-label'] ?? (typeof label === 'string' ? label : '')}
                // This is SAFE, as there is a constraint on `items` that prevents using keys
                // that do not correspond to array values.
                // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment, @typescript-eslint/no-unsafe-call
                defaultSelectedKeys={value?.map((item: FieldValues<Schema>[FieldName]) =>
                  items.indexOf(item),
                )}
                onSelectionChange={(selection) => {
                  onChange([...selection].map((key) => items[Number(key)]))
                }}
              >
                {items.map((item, i) => (
                  <MultiSelectorOption
                    key={i}
                    id={i}
                    value={{ item }}
                    label={children(item)}
                    variant={optionVariant}
                  />
                ))}
              </ListBox>
            )
          }}
        />
      </div>
      <FieldError />
    </Form.Field>
  )
})
