/** @file A form for changing the user's password. */
import * as z from 'zod'

import { ButtonGroup, Form, Input } from '#/components/AriaComponents'
import { passwordSchema, passwordWithPatternSchema } from '#/pages/authentication/schemas'
import { useAuth, useFullUserSession } from '#/providers/AuthProvider'
import { type GetText, useText } from '#/providers/TextProvider'
import { PASSWORD_REGEX } from '#/utilities/validation'
import SettingsAriaInput from './AriaInput'

/** Create the schema for this form. */
function createChangePasswordFormSchema(getText: GetText) {
  return z
    .object({
      username: z.string().email(getText('invalidEmailValidationError')),
      currentPassword: passwordSchema(getText),
      newPassword: passwordWithPatternSchema(getText),
      confirmNewPassword: z.string(),
    })
    .superRefine((object, context) => {
      if (
        PASSWORD_REGEX.test(object.newPassword) &&
        object.newPassword !== object.confirmNewPassword
      ) {
        context.addIssue({
          path: ['confirmNewPassword'],
          code: 'custom',
          message: getText('passwordMismatchError'),
        })
      }
    })
}

// ==========================
// === ChangePasswordForm ===
// ==========================

/** A form for changing the user's password. */
export default function ChangePasswordForm() {
  const { user } = useFullUserSession()
  const { changePassword } = useAuth()
  const { getText } = useText()

  return (
    <Form
      schema={createChangePasswordFormSchema(getText)}
      gap="none"
      onSubmit={({ currentPassword, newPassword }) => changePassword(currentPassword, newPassword)}
    >
      <Input hidden name="username" autoComplete="username" value={user.email} readOnly />
      <SettingsAriaInput
        data-testid="current-password-input"
        name="currentPassword"
        type="password"
        autoComplete="current-password"
        label={getText('currentPasswordLabel')}
        placeholder={getText('currentPasswordPlaceholder')}
      />
      <SettingsAriaInput
        data-testid="new-password-input"
        name="newPassword"
        type="password"
        label={getText('newPasswordLabel')}
        placeholder={getText('newPasswordPlaceholder')}
        autoComplete="new-password"
        description={getText('passwordValidationMessage')}
      />
      <SettingsAriaInput
        data-testid="confirm-new-password-input"
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
