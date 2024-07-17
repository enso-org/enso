/** @file A form for changing the user's password. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import SettingsInput from '#/components/styled/SettingsInput'

import * as eventModule from '#/utilities/event'
import * as uniqueString from '#/utilities/uniqueString'
import * as validation from '#/utilities/validation'

// ==========================
// === ChangePasswordForm ===
// ==========================

/** A form for changing the user's password. */
export default function ChangePasswordForm() {
  const { user } = authProvider.useFullUserSession()
  const { changePassword } = authProvider.useAuth()
  const { getText } = textProvider.useText()
  const [key, setKey] = React.useState('')
  const [currentPassword, setCurrentPassword] = React.useState('')
  const [newPassword, setNewPassword] = React.useState('')
  const [confirmNewPassword, setConfirmNewPassword] = React.useState('')

  const canSubmitPassword =
    currentPassword !== '' &&
    newPassword !== '' &&
    confirmNewPassword !== '' &&
    newPassword === confirmNewPassword &&
    validation.PASSWORD_REGEX.test(newPassword)
  const canCancel = currentPassword !== '' || newPassword !== '' || confirmNewPassword !== ''

  return (
    <aria.Form
      key={key}
      onSubmit={event => {
        event.preventDefault()
        setKey(uniqueString.uniqueString())
        setCurrentPassword('')
        setNewPassword('')
        setConfirmNewPassword('')
        void changePassword(currentPassword, newPassword)
      }}
    >
      <aria.Input hidden autoComplete="username" value={user.email} readOnly />
      <aria.TextField className="flex h-row gap-settings-entry" onChange={setCurrentPassword}>
        <aria.Label className="text my-auto w-change-password-settings-label">
          {getText('currentPasswordLabel')}
        </aria.Label>
        <SettingsInput
          type="password"
          autoComplete="current-password"
          placeholder={getText('currentPasswordPlaceholder')}
        />
      </aria.TextField>
      <aria.TextField
        className="flex h-row gap-settings-entry"
        onChange={setNewPassword}
        validate={value =>
          validation.PASSWORD_REGEX.test(value)
            ? true
            : value === ''
              ? ''
              : getText('passwordValidationError')
        }
      >
        <aria.Label className="text my-auto w-change-password-settings-label">
          {getText('newPasswordLabel')}
        </aria.Label>
        <SettingsInput
          type="password"
          placeholder={getText('newPasswordPlaceholder')}
          autoComplete="new-password"
        />
      </aria.TextField>
      <aria.TextField
        className="flex h-row gap-settings-entry"
        onChange={setConfirmNewPassword}
        validate={value =>
          value === newPassword ? true : value === '' ? '' : getText('passwordMismatchError')
        }
      >
        <aria.Label className="text my-auto w-change-password-settings-label">
          {getText('confirmNewPasswordLabel')}
        </aria.Label>
        <SettingsInput
          type="password"
          placeholder={getText('confirmNewPasswordPlaceholder')}
          autoComplete="new-password"
        />
      </aria.TextField>
      <ariaComponents.ButtonGroup>
        <ariaComponents.Button
          variant="submit"
          isDisabled={!canSubmitPassword}
          onPress={eventModule.submitForm}
        >
          {getText('change')}
        </ariaComponents.Button>
        <ariaComponents.Button
          variant="cancel"
          isDisabled={!canCancel}
          onPress={() => {
            setKey(uniqueString.uniqueString())
            setCurrentPassword('')
            setNewPassword('')
            setConfirmNewPassword('')
          }}
        >
          {getText('cancel')}
        </ariaComponents.Button>
      </ariaComponents.ButtonGroup>
    </aria.Form>
  )
}
