/** @file A selector for one or more items from a list of choices. */
import {
  CheckboxGroup as AriaCheckboxGroup,
  mergeProps,
  type CheckboxGroupProps as AriaCheckboxGroupProps,
} from '#/components/aria'
import { Field, type FieldVariantProps } from '#/components/AriaComponents/Form/components/Field'
import { useFormContext } from '#/components/AriaComponents/Form/components/hooks'
import type {
  FieldPath,
  FieldProps,
  FieldStateProps,
  TSchema,
} from '#/components/AriaComponents/Form/types'
import { mergeRefs } from '#/utilities/mergeRefs'
import { omit } from '#/utilities/object'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import {
  forwardRef,
  type CSSProperties,
  type ForwardedRef,
  type ReactElement,
  type ReactNode,
} from 'react'
import { Controller } from 'react-hook-form'
import type { TestIdProps } from '../types'
import { CheckboxGroupProvider } from './CheckboxContext'

/** Props for the {@link CheckboxGroup} component. */
export interface CheckboxGroupProps<
  Schema extends TSchema,
  FieldName extends FieldPath<Schema, readonly string[]>,
> extends FieldStateProps<AriaCheckboxGroupProps, Schema, FieldName, readonly string[]>,
    FieldProps,
    FieldVariantProps,
    Omit<VariantProps<typeof CHECKBOX_GROUP_STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  readonly className?: string
  readonly style?: CSSProperties
  readonly checkboxRef?: ForwardedRef<HTMLInputElement>
  readonly children: ReactNode | ((props: AriaCheckboxGroupProps) => ReactNode)
}

const CHECKBOX_GROUP_STYLES = tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: { fullWidth: { true: 'w-full' } },
})

/** A selector for one or more items from a list of choices. */
export const CheckboxGroup = forwardRef(
  <Schema extends TSchema, FieldName extends FieldPath<Schema, readonly string[]>>(
    props: CheckboxGroupProps<Schema, FieldName>,
    ref: ForwardedRef<HTMLDivElement>,
  ): ReactElement => {
    const {
      children,
      className,
      variants = CHECKBOX_GROUP_STYLES,
      form,
      defaultValue: defaultValueOverride,
      isDisabled = false,
      isRequired = false,
      isInvalid,
      isReadOnly = false,
      label,
      name,
      description,
      fullWidth = false,
      fieldVariants,
      contextualHelp,
      ...checkboxGroupProps
    } = props

    const formInstance = useFormContext(form)

    const styles = variants({ fullWidth, className })
    const testId = props['data-testid'] ?? props.testId

    return (
      <Controller
        name={name}
        control={formInstance.control}
        {...(defaultValueOverride != null && { defaultValue: defaultValueOverride })}
        render={({ field, fieldState }) => {
          const defaultValue = defaultValueOverride ?? formInstance.control._defaultValues[name]
          const invalid = isInvalid ?? fieldState.invalid
          return (
            <>
              <CheckboxGroupProvider
                name={name}
                field={field}
                defaultValue={defaultValue}
                onChange={(value) => {
                  field.onChange({ target: { value } })
                  void formInstance.trigger(name)
                }}
              >
                <AriaCheckboxGroup
                  {...mergeProps<AriaCheckboxGroupProps>()(omit(checkboxGroupProps, 'validate'), {
                    className: styles,
                    isInvalid: invalid,
                    isDisabled,
                    isReadOnly,
                    name,
                    defaultValue: defaultValue ?? [],
                  })}
                  ref={mergeRefs(ref, field.ref)}
                  data-testid={testId}
                >
                  {(renderProps) => (
                    <Field
                      name={name}
                      form={formInstance}
                      label={label}
                      description={description}
                      isRequired={isRequired}
                      fullWidth={fullWidth}
                      isInvalid={invalid}
                      variants={fieldVariants}
                      {...checkboxGroupProps}
                      contextualHelp={contextualHelp}
                    >
                      {typeof children === 'function' ? children(renderProps) : children}
                    </Field>
                  )}
                </AriaCheckboxGroup>
              </CheckboxGroupProvider>
            </>
          )
        }}
      />
    )
  },
)
