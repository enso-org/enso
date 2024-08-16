/**
 * @file
 *
 * Checkboxes allow users to select multiple items from a list of individual items, or to mark one individual item as selected.
 */
import type { CheckboxProps as AriaCheckboxProps } from '#/components/aria'
import { Checkbox as AriaCheckbox } from '#/components/aria'
import { mergeRefs } from '#/utilities/mergeRefs'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import type { Variants } from 'framer-motion'
import { motion } from 'framer-motion'
import type {
  CSSProperties,
  ForwardedRef,
  MutableRefObject,
  ReactElement,
  RefAttributes,
} from 'react'
import { forwardRef } from 'react'
import invariant from 'tiny-invariant'
import { useStore } from 'zustand'
import type {
  FieldPath,
  FieldStateProps,
  FieldValues,
  FormInstance,
  TSchema,
  UseFormRegisterReturn,
} from '../Form'
import { Form } from '../Form'
import { Text } from '../Text'
import type { TestIdProps } from '../types'
import { useCheckboxContext } from './CheckboxContext'
import { CheckboxGroup } from './CheckboxGroup'

/**
 * Props for the {@link Checkbox} component.
 */
export type CheckboxProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> = Omit<VariantProps<typeof CHECKBOX_STYLES>, 'isDisabled' | 'isInvalid'> &
  TestIdProps & {
    readonly className?: string
    readonly style?: CSSProperties
    readonly checkboxRef?: MutableRefObject<HTMLInputElement>
  } & (
    | CheckboxGroupCheckboxProps
    | StandaloneCheckboxProps<Schema, TFieldValues, TFieldName, TTransformedValues>
  )

/**
 *
 */
interface CheckboxGroupCheckboxProps extends AriaCheckboxProps {
  readonly value: string
  readonly form?: never
  readonly name?: never
}

/**
 *
 */
interface StandaloneCheckboxProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends FieldStateProps<
    AriaCheckboxProps,
    Schema,
    TFieldValues,
    TFieldName,
    TTransformedValues
  > {}

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

export const TICK_VARIANTS: Variants = {
  checked: {
    pathLength: 1,
    opacity: 1,
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    transition: { type: 'tween', duration: 0.2, easings: 'circIn' },
  },
  unchecked: {
    pathLength: 0,
    opacity: 0,
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    transition: { type: 'tween', duration: 0.2, easings: 'circOut' },
  },
}

/**
 * Checkboxes allow users to select multiple items from a list of individual items, or to mark one individual item as selected.
 */
// eslint-disable-next-line no-restricted-syntax
export const Checkbox = forwardRef(function Checkbox<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  props: CheckboxProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
  ref: ForwardedRef<HTMLLabelElement>,
) {
  const {
    checkboxRef,
    variants = CHECKBOX_STYLES,
    isDisabled = false,
    isIndeterminate = false,
    isInvalid = false,
    isReadOnly = false,
    className,
    children,
    size,
    form,
  } = props

  const { store, removeSelected, addSelected } = useCheckboxContext()

  // eslint-disable-next-line react-hooks/rules-of-hooks,no-restricted-syntax
  const formInstance = (form ?? Form.useFormContext()) as FormInstance<
    Schema,
    TFieldValues,
    TTransformedValues
  >

  const { isSelected, field, onChange, name } = useStore(store, (state) => {
    const { insideGroup } = state

    if (insideGroup) {
      const value = props.value

      invariant(value != null, 'Checkbox must have a value when inside a group')

      return {
        isSelected: state.selected.has(value),
        // This is safe, because the name is handled by the `CheckboxGroup` component
        // and checked there
        // eslint-disable-next-line no-restricted-syntax
        field: state.field as UseFormRegisterReturn<Schema, TFieldValues, TFieldName>,
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
    } else {
      invariant(props.name != null, 'Checkbox must have a name when outside a group')

      // eslint-disable-next-line no-restricted-syntax
      const fieldInstance = formInstance.register(props.name) as UseFormRegisterReturn<
        Schema,
        TFieldValues,
        TFieldName
      >

      return {
        field: fieldInstance,
        name: props.name,
        isSelected: props.isSelected ?? false,
        onChange: (checked: boolean) => {
          void fieldInstance.onChange({ target: { value: checked } })
          void formInstance.trigger(props.name)
        },
      }
    }
  })

  const { fieldState } = Form.useField({
    name,
    // eslint-disable-next-line no-restricted-syntax
    form: formInstance as unknown as Parameters<typeof Form.useField>[0]['form'],
  })

  const classes = variants({
    isReadOnly: isReadOnly,
    isInvalid: isInvalid || fieldState.invalid,
    isDisabled: isDisabled || field.isDisabled,
    size,
  })

  return (
    <AriaCheckbox
      ref={mergeRefs(ref, field.ref)}
      {...props}
      /* eslint-disable-next-line no-restricted-syntax */
      inputRef={checkboxRef as MutableRefObject<HTMLInputElement>}
      className={(renderProps) => classes.base({ className, isSelected: renderProps.isSelected })}
      isSelected={isSelected}
      onChange={onChange}
      onBlur={field.onBlur}
      isIndeterminate={isIndeterminate}
      isInvalid={isInvalid || fieldState.invalid}
      isDisabled={isDisabled || (field.isDisabled ?? false)}
      isReadOnly={isReadOnly}
      isRequired={field.required ?? false}
    >
      {(renderProps) => (
        <>
          <motion.svg
            xmlns="http://www.w3.org/2000/svg"
            viewBox="0 0 16 16"
            className={classes.icon({ isSelected: renderProps.isSelected })}
            initial={false}
            animate={renderProps.isSelected ? 'checked' : 'unchecked'}
            role="presentation"
          >
            <motion.path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth="2"
              stroke="currentColor"
              fill="none"
              d={isIndeterminate ? 'M5 8H11' : 'M4 8.4L6.5 10.9L9.25 8.15L12 5.4'}
              variants={TICK_VARIANTS}
            />
          </motion.svg>

          <Text variant="body" color="current">
            {typeof children === 'function' ? children(renderProps) : children}
          </Text>
        </>
      )}
    </AriaCheckbox>
  )
}) as unknown as (<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  props: CheckboxProps<Schema, TFieldValues, TFieldName, TTransformedValues> &
    RefAttributes<HTMLInputElement>,
) => ReactElement) & {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Group: typeof CheckboxGroup
}

Checkbox.Group = CheckboxGroup
