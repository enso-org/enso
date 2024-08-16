/**
 * @file
 *
 * A CheckboxGroup allows users to select one or more items from a list of choices.
 */
import type { CheckboxGroupProps as AriaCheckboxGroupProps } from '#/components/aria'
import { CheckboxGroup as AriaCheckboxGroup, mergeProps } from '#/components/aria'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { mergeRefs } from '#/utilities/mergeRefs'
import { omit } from '#/utilities/object'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { tv } from '#/utilities/tailwindVariants'
import type { CSSProperties, ForwardedRef, ReactElement, RefAttributes } from 'react'
import { forwardRef } from 'react'
import type { FieldVariantProps } from '../Form'
import {
  Form,
  type FieldPath,
  type FieldProps,
  type FieldStateProps,
  type FieldValues,
  type TSchema,
} from '../Form'
import type { TestIdProps } from '../types'
import { CheckboxGroupProvider } from './CheckboxContext'

/**
 * Props for the {@link CheckboxGroupProps} component.
 */
export interface CheckboxGroupProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends FieldStateProps<
      AriaCheckboxGroupProps,
      Schema,
      TFieldValues,
      TFieldName,
      TTransformedValues
    >,
    FieldProps,
    FieldVariantProps,
    Omit<VariantProps<typeof CHECKBOX_GROUP_STYLES>, 'disabled' | 'invalid'>,
    TestIdProps {
  readonly className?: string
  readonly style?: CSSProperties
  readonly checkboxRef?: ForwardedRef<HTMLInputElement>
}

const CHECKBOX_GROUP_STYLES = tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: { fullWidth: { true: 'w-full' } },
})

/**
 * A CheckboxGroup allows users to select one or more items from a list of choices.
 */
// eslint-disable-next-line no-restricted-syntax
export const CheckboxGroup = forwardRef(
  <
    Schema extends TSchema,
    TFieldValues extends FieldValues<Schema>,
    TFieldName extends FieldPath<Schema, TFieldValues>,
    TTransformedValues extends FieldValues<Schema> | undefined = undefined,
  >(
    props: CheckboxGroupProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
    ref: ForwardedRef<HTMLFieldSetElement>,
  ): ReactElement => {
    const {
      children,
      className,
      variants = CHECKBOX_GROUP_STYLES,
      form,
      defaultValue,
      isDisabled = false,
      isRequired = false,
      isInvalid = false,
      isReadOnly = false,
      label,
      name,
      description,
      fullWidth = false,
      testId,
      fieldVariants,
      ...checkboxGroupProps
    } = props

    const { fieldState, formInstance } = Form.useField({
      name,
      isDisabled,
      form,
      defaultValue,
    })

    const field = formInstance.register(name, { disabled: isDisabled, required: isRequired })

    const invalid = isInvalid || fieldState.invalid

    const styles = variants({ fullWidth, className })

    return (
      <CheckboxGroupProvider
        name={name}
        field={field}
        onChange={useEventCallback(
          (selected) =>
            void field
              .onChange({ target: { name, value: selected } })
              .then(() => formInstance.trigger(name)),
        )}
      >
        <AriaCheckboxGroup
          {...mergeProps<AriaCheckboxGroupProps>()(omit(checkboxGroupProps, 'validate'), {
            className: styles,
            isInvalid,
            isDisabled,
            isReadOnly,
            name,
            testId,
          })}
          ref={mergeRefs(ref, field.ref)}
        >
          {(renderProps) => (
            <Form.Field
              name={name}
              form={formInstance}
              label={label}
              description={description}
              isRequired={isRequired}
              fullWidth={fullWidth}
              isInvalid={invalid}
              variants={fieldVariants}
              {...checkboxGroupProps}
            >
              {typeof children === 'function' ? children(renderProps) : children}
            </Form.Field>
          )}
        </AriaCheckboxGroup>
      </CheckboxGroupProvider>
    )
  },
) as <
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends FieldPath<Schema, TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(
  props: CheckboxGroupProps<Schema, TFieldValues, TFieldName, TTransformedValues> &
    RefAttributes<HTMLDivElement>,
) => ReactElement
