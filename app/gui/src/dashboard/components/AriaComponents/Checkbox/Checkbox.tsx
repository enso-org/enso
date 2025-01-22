/**
 * @file
 *
 * Checkboxes allow users to select multiple items from a list of individual items, or to mark one individual item as selected.
 */
import type { CheckboxProps as AriaCheckboxProps } from '#/components/aria'
import { Checkbox as AriaCheckbox, CheckboxGroupStateContext } from '#/components/aria'
import { mergeRefs, useMergedRef } from '#/utilities/mergeRefs'
import { forwardRef } from '#/utilities/react'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import { useStore } from '#/utilities/zustand'
import {
  useContext,
  type CSSProperties,
  type ForwardedRef,
  type MutableRefObject,
  type ReactElement,
  type RefAttributes,
} from 'react'
import type { CheckboxGroupState } from 'react-stately'
import invariant from 'tiny-invariant'
import { Check } from '../Check/Check'
import type {
  FieldPath,
  FieldProps,
  FieldStateProps,
  FieldVariantProps,
  TSchema,
  UseFormRegisterReturn,
} from '../Form'
import { Form } from '../Form'
import { Text } from '../Text'
import type { TestIdProps } from '../types'
import { CheckboxStandaloneProvider, useCheckboxContext } from './CheckboxContext'
import { CheckboxGroup } from './CheckboxGroup'

/** Props for the {@link Checkbox} component. */
export type CheckboxProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, boolean>,
> = Omit<VariantProps<typeof CHECKBOX_STYLES>, 'isDisabled' | 'isInvalid'> &
  TestIdProps & {
    readonly className?: string
    readonly style?: CSSProperties
    readonly checkboxRef?: MutableRefObject<HTMLInputElement>
  } & (CheckboxGroupCheckboxProps | StandaloneCheckboxProps<Schema, TFieldName>)

/** Props for the {@link Checkbox} component when used inside a {@link CheckboxGroup}. */
interface CheckboxGroupCheckboxProps extends AriaCheckboxProps {
  readonly value: string
  readonly form?: never
  readonly name?: never
}

/** Props for the {@link Checkbox} component when used outside of a {@link CheckboxGroup}. */
type StandaloneCheckboxProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, boolean>,
> = FieldProps & FieldStateProps<AriaCheckboxProps, Schema, TFieldName, boolean> & FieldVariantProps

export const CHECKBOX_STYLES = tv({
  base: 'group flex gap-2 items-center cursor-pointer select-none',
  variants: {
    isInvalid: {
      true: {
        base: 'text-danger',
        icon: 'border-danger focus-within:border-danger focus-within:outline-danger',
      },
    },
    isReadOnly: {
      true: { icon: 'bg-primary/50 border-primary/50' },
    },
    isDisabled: {
      true: { icon: 'bg-primary/30 border-primary/30 cursor-not-allowed' },
      false: '',
    },
    isSelected: {
      true: { icon: 'bg-primary text-white' },
      false: { icon: 'bg-transparent text-primary' },
    },
    size: { medium: { icon: 'w-4 h-4' } },
  },
  slots: {
    icon: [
      'border-[0.5px] rounded-md transition-[outline-offset,border-width] duration-200',
      'outline -outline-offset-2 outline-transparent group-focus-visible:outline-offset-0 group-focus-visible:outline-primary',
      'border-primary group-selected:border-transparent',
      'group-pressed:border',
      'shrink-0',
    ],
  },
  defaultVariants: {
    size: 'medium',
  },
  compoundVariants: [
    {
      isInvalid: true,
      isSelected: true,
      class: {
        icon: 'bg-danger border-danger focus-within:border-danger focus-within:outline-danger',
      },
    },
  ],
})

/** Checkboxes allow users to select multiple items from a list of individual items, or to mark one individual item as selected. */
// eslint-disable-next-line no-restricted-syntax
export const Checkbox = forwardRef(function Checkbox<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, boolean>,
>(props: CheckboxProps<Schema, TFieldName>, ref: ForwardedRef<HTMLLabelElement>) {
  const { form, name } = props

  const { store } = useCheckboxContext()
  const formInstance = Form.useFormContext(form)

  const isInsideGroup = useStore(store, (state) => state.insideGroup)

  if (!isInsideGroup) {
    // This should never happen, because a standalone checkbox should always have a name
    // and it specified in the props
    invariant(name != null, 'Checkbox must have a name when placed inside a group')

    const {
      defaultValue: defaultValueOverride,
      isInvalid,
      fieldVariants,
      // eslint-disable-next-line @typescript-eslint/naming-convention
      className: _,
      // eslint-disable-next-line @typescript-eslint/naming-convention
      style: __,
      ...fieldProps

      // This is safe, because we know that the checkbox is standalone, and
      // name is specified in the props.
      // eslint-disable-next-line no-restricted-syntax
    } = props as StandaloneCheckboxProps<Schema, TFieldName>

    return (
      <Form.Controller
        name={name}
        control={formInstance.control}
        {...(defaultValueOverride != null && { defaultValue: defaultValueOverride })}
        render={({ field, fieldState }) => {
          const defaultValue = defaultValueOverride ?? formInstance.control._defaultValues[name]
          return (
            <>
              <CheckboxStandaloneProvider
                name={name}
                field={field}
                defaultValue={defaultValue}
                onChange={(value) => {
                  field.onChange({ target: { value } })
                  void formInstance.trigger(name)
                }}
              >
                <Form.Field
                  {...fieldProps}
                  form={formInstance}
                  name={name}
                  isInvalid={isInvalid ?? fieldState.invalid}
                  variants={fieldVariants}
                >
                  <CheckboxInternal ref={ref} value={name} {...props} />
                </Form.Field>
              </CheckboxStandaloneProvider>
            </>
          )
        }}
      />
    )
  }

  return <CheckboxInternal ref={ref} {...props} />
}) as unknown as (<Schema extends TSchema, TFieldName extends FieldPath<Schema, boolean>>(
  props: CheckboxProps<Schema, TFieldName> & RefAttributes<HTMLLabelElement>,
) => ReactElement) & {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Group: typeof CheckboxGroup
}

/**
 * Internal props for the {@link Checkbox} component.
 */
type CheckboxInternalProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, boolean>,
> = Omit<CheckboxProps<Schema, TFieldName>, 'name'> & {
  name?: string
}

// eslint-disable-next-line no-restricted-syntax
const CheckboxInternal = forwardRef(function CheckboxInternal<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, boolean>,
>(props: CheckboxInternalProps<Schema, TFieldName>, ref: ForwardedRef<HTMLLabelElement>) {
  const {
    variants = CHECKBOX_STYLES,
    isDisabled = false,
    isIndeterminate = false,
    isReadOnly = false,
    checkboxRef,
    isInvalid,
    className,
    children,
    size,
    form,
  } = props

  const { store, removeSelected, addSelected } = useCheckboxContext()
  // This is safe, because we're intentionally widening the type
  // eslint-disable-next-line no-restricted-syntax
  const groupState = useContext(CheckboxGroupStateContext) as CheckboxGroupState | undefined

  const formInstance = Form.useFormContext(form)

  const { isSelected, field, onChange, name } = useStore(store, (state) => {
    const { insideGroup } = state

    if (insideGroup) {
      const value = props.value

      invariant(value != null, '`Checkbox` must have a value when placed inside a group')

      return {
        isSelected: state.selected.has(value),
        // This is safe, because the name is handled by the `CheckboxGroup` component
        // and checked there
        // eslint-disable-next-line no-restricted-syntax
        field: state.field as UseFormRegisterReturn<Schema, TFieldName, boolean>,
        // eslint-disable-next-line no-restricted-syntax
        name: state.name as TFieldName,
        onChange: (checked: boolean) => {
          if (checked) {
            addSelected(value)
          } else {
            removeSelected(value)
          }
        },
      }
    }

    invariant(
      false,
      "CheckboxInternal can't be placed outside of either CheckboxGroup or CheckboxStandaloneProvider",
    )
  })

  const { hasError: fieldStateInvalid } = Form.useFieldState({
    name,
    // eslint-disable-next-line no-restricted-syntax
    form: formInstance as unknown as Parameters<typeof Form.useField>[0]['form'],
  })

  const invalid = isInvalid ?? groupState?.isInvalid ?? fieldStateInvalid

  const classes = variants({
    isReadOnly: isReadOnly,
    isInvalid: invalid,
    isDisabled: isDisabled || field.disabled,
    size,
  })

  const testId = props['data-testid'] ?? props['testId']

  return (
    <AriaCheckbox
      ref={(el) => {
        mergeRefs(ref, field.ref)(el)
      }}
      {...props}
      inputRef={useMergedRef(checkboxRef, (input) => {
        // Hack to remove the `data-testid` attribute from the input element
        // react-aria-components adds this attribute, but it is a duplicate of the label's `data-testid`
        // which messes up the test selectors
        if (input != null) {
          delete input.dataset.testid
        }
      })}
      className={(renderProps) => classes.base({ className, isSelected: renderProps.isSelected })}
      isSelected={isSelected}
      onChange={onChange}
      onBlur={field.onBlur}
      isIndeterminate={isIndeterminate}
      isInvalid={invalid}
      isDisabled={isDisabled || (field.disabled ?? false)}
      isReadOnly={isReadOnly}
      isRequired={field.required ?? false}
      data-testid={testId}
    >
      {(renderProps) => (
        <>
          <Check
            color={renderProps.isInvalid ? 'error' : 'primary'}
            isSelected={renderProps.isSelected}
            isPressed={renderProps.isPressed}
            isIndeterminate={isIndeterminate}
            className={classes.icon({ isSelected: renderProps.isSelected })}
          />

          <Text variant="body" color="current">
            {typeof children === 'function' ? children(renderProps) : children}
          </Text>
        </>
      )}
    </AriaCheckbox>
  )
}) as unknown as (<Schema extends TSchema, TFieldName extends FieldPath<Schema, boolean>>(
  props: CheckboxInternalProps<Schema, TFieldName> & RefAttributes<HTMLLabelElement>,
) => ReactElement) & {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Group: typeof CheckboxGroup
}

Checkbox.Group = CheckboxGroup
