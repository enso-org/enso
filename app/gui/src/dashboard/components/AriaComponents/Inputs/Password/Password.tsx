/** @file A component wrapping {@link Input} with the ability to show and hide password. */
import { useState } from 'react'

import EyeIcon from '#/assets/eye.svg'
import EyeCrossedIcon from '#/assets/eye_crossed.svg'
import {
  Button,
  Form,
  Input,
  type FieldPath,
  type InputProps,
  type TSchema,
} from '#/components/AriaComponents'
import { AnimatePresence, motion } from 'framer-motion'

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

  const form = Form.useFormContext(props.form)

  return (
    <Input<Schema, TFieldName, string>
      {...props}
      type={showPassword ? 'text' : 'password'}
      addonEnd={
        <>
          {props.addonEnd}

          <Form.FieldValue<Schema, TFieldName, string> form={form} name={props.name}>
            {(value) => (
              <AnimatePresence>
                {value != null && value.length > 0 && (
                  <motion.div
                    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                    initial={{ opacity: 0, x: 10, rotateY: 30 }}
                    animate={{ opacity: 1, x: 0, rotateY: 0 }}
                    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                    exit={{ opacity: 0, x: 10, rotateY: 30 }}
                    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                    transition={{ duration: 0.2, ease: 'easeInOut' }}
                  >
                    <Button
                      size="medium"
                      variant="icon"
                      extraClickZone
                      icon={showPassword ? EyeIcon : EyeCrossedIcon}
                      onPress={() => {
                        setShowPassword(!showPassword)
                      }}
                    />
                  </motion.div>
                )}
              </AnimatePresence>
            )}
          </Form.FieldValue>
        </>
      }
    />
  )
}
