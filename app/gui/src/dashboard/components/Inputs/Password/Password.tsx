/** @file A component wrapping {@link Input} with the ability to show and hide password. */
import { Button } from '#/components/Button'
import { Form, type FieldPath, type TSchema } from '#/components/Form'
import { Input, type InputProps } from '#/components/Inputs/Input'
import { useState } from 'react'

/** Props for a {@link Password}. */
export interface PasswordProps<Schema extends TSchema, TFieldName extends FieldPath<Schema, string>>
  extends Omit<InputProps<Schema, TFieldName, string>, 'type'> {}

/** A component wrapping {@link Input} with the ability to show and hide password. */
export function Password<Schema extends TSchema, TFieldName extends FieldPath<Schema, string>>(
  props: PasswordProps<Schema, TFieldName>,
) {
  const [showPassword, setShowPassword] = useState(false)

  const form = Form.useFormContext(props.form)

  return (
    <Input<Schema, TFieldName, string>
      {...props}
      type={showPassword ? 'text' : 'password'}
      addonEnd={
        <>
          {props.addonEnd}

          <Form.FieldValue<Schema, TFieldName, string> form={form} name={props.name}>
            {(value) =>
              value != null && value.length > 0 ?
                <Button
                  size="medium"
                  variant="icon"
                  extraClickZone
                  icon={showPassword ? 'eye' : 'eye_crossed'}
                  onPress={() => {
                    setShowPassword(!showPassword)
                  }}
                />
              : null
            }
          </Form.FieldValue>
        </>
      }
    />
  )
}
