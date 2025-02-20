/** @file A radio group. */
import * as React from 'react'

import * as aria from '#/components/aria'

import * as mergeRefs from '#/utilities/mergeRefs'
import * as twv from '#/utilities/tailwindVariants'

import { omit } from '#/utilities/object'
import { forwardRef } from '#/utilities/react'
import type { FieldVariantProps } from '../Form'
import { Form, type FieldPath, type FieldProps, type FieldStateProps, type TSchema } from '../Form'
import { RadioGroupProvider } from './RadioGroupContext'

/** Props for {@link RadioGroup}. */
export interface RadioGroupProps<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, string>,
> extends FieldStateProps<
      Omit<aria.AriaRadioGroupProps, 'description' | 'label'>,
      Schema,
      TFieldName,
      string
    >,
    twv.VariantProps<typeof RADIO_GROUP_STYLES>,
    FieldProps,
    FieldVariantProps {
  readonly children?: React.ReactNode
  readonly className?: string
  readonly style?: React.CSSProperties
}

export const RADIO_GROUP_STYLES = twv.tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: { fullWidth: { true: 'w-full' } },
})

// This is a function, even though it does not contain function syntax.
// eslint-disable-next-line no-restricted-syntax
const useStringField = Form.makeUseField<string>()

/** A radio group. */
export const RadioGroup = forwardRef(function RadioGroup<
  Schema extends TSchema,
  TFieldName extends FieldPath<Schema, string>,
>(props: RadioGroupProps<Schema, TFieldName>, ref: React.ForwardedRef<HTMLDivElement>) {
  const {
    children,
    isRequired = false,
    isReadOnly = false,
    isDisabled = false,
    isInvalid = false,
    name,
    className,
    form,
    defaultValue,
    label,
    description,
    fullWidth,
    variants = RADIO_GROUP_STYLES,
    fieldVariants,
    ...radioGroupProps
  } = props

  const { field, fieldState, formInstance } = useStringField({
    name,
    isDisabled,
    form,
    defaultValue,
  })

  const invalid = isInvalid || fieldState.invalid

  const base = variants({ fullWidth, className })

  return (
    <aria.RadioGroup
      ref={(el) => {
        mergeRefs.mergeRefs(ref, field.ref)(el)
      }}
      {...aria.mergeProps<aria.RadioGroupProps>()(omit(radioGroupProps, 'validate'), {
        name: field.name,
        value: field.value,
        isDisabled: field.disabled ?? isDisabled,
        onChange: field.onChange,
        onBlur: field.onBlur,
        className: base,
        isRequired,
        isReadOnly,
        isInvalid: invalid,
      })}
    >
      <RadioGroupProvider>
        <Form.Field
          name={name}
          form={formInstance}
          label={label}
          description={description}
          fullWidth={fullWidth}
          isInvalid={invalid}
          variants={fieldVariants}
          isRequired={isRequired}
        >
          {children}
        </Form.Field>
      </RadioGroupProvider>
    </aria.RadioGroup>
  )
})
