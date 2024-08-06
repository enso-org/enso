/** @file A form for changing the user's password. */
import * as React from 'react'

import * as z from 'zod'

import { ButtonGroup, Form, Input } from '#/components/AriaComponents'
import { useAuth, useNonPartialUserSession } from '#/providers/AuthProvider'
import { type GetText, useText } from '#/providers/TextProvider'

import { passwordWithPatternSchema } from '#/pages/authentication/schemas'

/** Create the schema for this form. */
function createChangePasswordFormSchema(getText: GetText) {
  return z
    .object({
      username: z.string().email(getText('invalidEmailValidationError')),
      currentPassword: passwordWithPatternSchema(getText),
      newPassword: passwordWithPatternSchema(getText),
      confirmNewPassword: z.string(),
    })
    .refine(
      (object) => object.newPassword === object.confirmNewPassword,
      getText('passwordMismatchError'),
    )
}

// ==========================
// === ChangePasswordForm ===
// ==========================

/** A form for changing the user's password. */
export default function ChangePasswordForm() {
  const { user } = useNonPartialUserSession()
  const { changePassword } = useAuth()
  const { getText } = useText()

  return (
    <Form
      schema={createChangePasswordFormSchema(getText)}
      onSubmit={({ currentPassword, newPassword }) => changePassword(currentPassword, newPassword)}
    >
      <Input hidden name="username" autoComplete="username" value={user.email} readOnly />
      <Input
        name="currentPassword"
        type="password"
        autoComplete="current-password"
        label={getText('currentPasswordLabel')}
        placeholder={getText('currentPasswordPlaceholder')}
      />
      <Input
        name="newPassword"
        type="password"
        label={getText('newPasswordLabel')}
        placeholder={getText('newPasswordPlaceholder')}
        autoComplete="new-password"
      />
      <Input
        name="confirmNewPassword"
        type="password"
        label={getText('confirmNewPasswordLabel')}
        placeholder={getText('confirmNewPasswordPlaceholder')}
        autoComplete="new-password"
      />

      <Form.FormError />
      <ButtonGroup>
        <Form.Submit>{getText('change')}</Form.Submit>
        <Form.Reset>{getText('cancel')}</Form.Reset>
      </ButtonGroup>
    </Form>
  )
}
