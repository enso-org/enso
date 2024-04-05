/** @file Settings section for changing password. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import ButtonRow from '#/components/styled/ButtonRow'
import SettingsInput from '#/components/styled/settings/SettingsInput'
import SettingsSection from '#/components/styled/settings/SettingsSection'
import UnstyledButton from '#/components/UnstyledButton'

import * as eventModule from '#/utilities/event'
import * as uniqueString from '#/utilities/uniqueString'
import * as validation from '#/utilities/validation'

// =====================================
// === ChangePasswordSettingsSection ===
// =====================================

/** Settings section for changing password. */
export default function ChangePasswordSettingsSection() {
  const { user } = authProvider.useNonPartialUserSession()
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
    <SettingsSection title={getText('changePassword')}>
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
        <aria.Input hidden autoComplete="username" value={user?.value.email} readOnly />
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
            value === '' || validation.PASSWORD_REGEX.test(value)
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
          validate={newValue =>
            newValue === '' || newValue === newPassword ? '' : getText('passwordMismatchError')
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
        <ButtonRow>
          <UnstyledButton
            isDisabled={!canSubmitPassword}
            className={`settings-value rounded-full bg-invite font-medium text-white selectable enabled:active`}
            onPress={eventModule.submitForm}
          >
            {getText('change')}
          </UnstyledButton>
          <UnstyledButton
            isDisabled={!canCancel}
            className="settings-value rounded-full bg-selected-frame font-medium selectable enabled:active"
            onPress={() => {
              setKey(uniqueString.uniqueString())
              setCurrentPassword('')
              setNewPassword('')
              setConfirmNewPassword('')
            }}
          >
            {getText('cancel')}
          </UnstyledButton>
        </ButtonRow>
      </aria.Form>
    </SettingsSection>
  )
}
