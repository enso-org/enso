/** @file A component wrapping {@link Input} with the ability to show and hide password. */
import { useState } from 'react'
import type { Path } from 'react-hook-form'

import EyeIcon from '#/assets/eye.svg'
import EyeCrossedIcon from '#/assets/eye_crossed.svg'
import {
  Button,
  Input,
  type FieldValues,
  type InputProps,
  type TSchema,
} from '#/components/AriaComponents'

// ================
// === Password ===
// ================

/** Props for a {@link Password}. */
export interface PasswordProps<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends Path<TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
> extends Omit<
    InputProps<Schema, TFieldValues, TFieldName, TTransformedValues>,
    'addonMiddle' | 'type'
  > {}

/** A component wrapping {@link Input} with the ability to show and hide password. */
export function Password<
  Schema extends TSchema,
  TFieldValues extends FieldValues<Schema>,
  TFieldName extends Path<TFieldValues>,
  TTransformedValues extends FieldValues<Schema> | undefined = undefined,
>(props: PasswordProps<Schema, TFieldValues, TFieldName, TTransformedValues>) {
  const [showPassword, setShowPassword] = useState(false)

  return (
    <Input
      {...props}
      type={showPassword ? 'text' : 'password'}
      addonMiddle={
        <Button
          size="medium"
          variant="icon"
          icon={showPassword ? EyeIcon : EyeCrossedIcon}
          className="absolute right-0 top-1/2 -translate-y-1/2"
          extraClickZone={false}
          onPress={() => {
            setShowPassword(!showPassword)
          }}
        />
      }
    />
  )
}
