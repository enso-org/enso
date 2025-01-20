/**
 * @file
 *
 * A radio group component.
 */
import * as React from 'react'

import * as aria from '#/components/aria'

import * as mergeRefs from '#/utilities/mergeRefs'
import * as twv from '#/utilities/tailwindVariants'

import { omit } from '#/utilities/object'
import { forwardRef } from '#/utilities/react'
import type { FieldVariantProps } from '../Form'
import * as formComponent from '../Form'
import * as radioGroupContext from './RadioGroupContext'

/** Props for {@link RadioGroup}. */
export interface RadioGroupProps<
  Schema extends formComponent.TSchema,
  TFieldName extends formComponent.FieldPath<Schema>,
> extends formComponent.FieldStateProps<
      Omit<aria.AriaRadioGroupProps, 'description' | 'label'>,
      Schema,
      TFieldName
    >,
    twv.VariantProps<typeof RADIO_GROUP_STYLES>,
    formComponent.FieldProps,
    FieldVariantProps {
  readonly children?: React.ReactNode
  readonly className?: string
}

export const RADIO_GROUP_STYLES = twv.tv({
  base: 'flex flex-col gap-0.5 items-start',
  variants: { fullWidth: { true: 'w-full' } },
})

/** A radio group component. */

export const RadioGroup = forwardRef(function RadioGroup<
  Schema extends formComponent.TSchema,
  TFieldName extends formComponent.FieldPath<Schema>,
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

  const { field, fieldState, formInstance } = formComponent.Form.useField({
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
      <radioGroupContext.RadioGroupProvider>
        <formComponent.Form.Field
          name={name}
          form={formInstance}
          label={label}
          description={description}
          fullWidth={fullWidth}
          isInvalid={invalid}
          variants={fieldVariants}
          {...radioGroupProps}
        >
          {children}
        </formComponent.Form.Field>
      </radioGroupContext.RadioGroupProvider>
    </aria.RadioGroup>
  )
})
