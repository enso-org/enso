/** @file A component wrapping {@link Input} with the ability to show and hide password. */
import { useState } from 'react'

import EyeIcon from '#/assets/eye.svg'
import EyeCrossedIcon from '#/assets/eye_crossed.svg'
import {
  Button,
  Input,
  type FieldPath,
  type InputProps,
  type TSchema,
} from '#/components/AriaComponents'

// ================
// === Password ===
// ================

/** Props for a {@link Password}. */
export interface PasswordProps<Schema extends TSchema, TFieldName extends FieldPath<Schema, string>>
  extends Omit<InputProps<Schema, TFieldName, string>, 'type'> {}

/** A component wrapping {@link Input} with the ability to show and hide password. */
export function Password<Schema extends TSchema, TFieldName extends FieldPath<Schema, string>>(
  props: PasswordProps<Schema, TFieldName>,
) {
  const [showPassword, setShowPassword] = useState(false)

  return (
    <Input<Schema, TFieldName, string>
      {...props}
      type={showPassword ? 'text' : 'password'}
      addonEnd={
        <>
          {props.addonEnd}

          <Button
            size="medium"
            variant="icon"
            extraClickZone
            icon={showPassword ? EyeIcon : EyeCrossedIcon}
            onPress={() => {
              setShowPassword(!showPassword)
            }}
          />
        </>
      }
    />
  )
}
