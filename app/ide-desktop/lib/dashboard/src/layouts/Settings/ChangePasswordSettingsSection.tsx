/** @file Settings section for changing password. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import SettingsInput from '#/layouts/Settings/SettingsInput'

import * as aria from '#/components/aria'
import ButtonRow from '#/components/styled/ButtonRow'
import FocusArea from '#/components/styled/FocusArea'
import UnstyledButton from '#/components/styled/UnstyledButton'

import * as uniqueString from '#/utilities/uniqueString'
import * as validation from '#/utilities/validation'

// =====================================
// === ChangePasswordSettingsSection ===
// =====================================

/** Settings section for changing password. */
export default function ChangePasswordSettingsSection() {
  const { changePassword } = authProvider.useAuth()
  const { getText } = textProvider.useText()
  const [passwordFormKey, setPasswordFormKey] = React.useState('')
  const [currentPassword, setCurrentPassword] = React.useState('')
  const [newPassword, setNewPassword] = React.useState('')
  const [confirmNewPassword, setConfirmNewPassword] = React.useState('')

  const canSubmitPassword =
    currentPassword !== '' &&
    newPassword !== '' &&
    confirmNewPassword !== '' &&
    newPassword === confirmNewPassword &&
    validation.PASSWORD_REGEX.test(newPassword)

  return (
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div ref={ref} key={passwordFormKey} {...innerProps}>
          <aria.Heading level={2} className="settings-subheading">
            {getText('changePassword')}
          </aria.Heading>
          <aria.TextField className="flex h-row gap-settings-entry">
            <aria.Label className="text my-auto w-change-password-settings-label">
              {getText('currentPasswordLabel')}
            </aria.Label>
            <SettingsInput
              type="password"
              initialValue=""
              placeholder={getText('currentPasswordPlaceholder')}
              onChange={event => {
                setCurrentPassword(event.currentTarget.value)
              }}
            />
          </aria.TextField>
          <aria.TextField className="flex h-row gap-settings-entry">
            <aria.Label className="text my-auto w-change-password-settings-label">
              {getText('newPasswordLabel')}
            </aria.Label>
            <SettingsInput
              type="password"
              initialValue=""
              placeholder={getText('newPasswordPlaceholder')}
              onChange={event => {
                const newValue = event.currentTarget.value
                setNewPassword(newValue)
                event.currentTarget.setCustomValidity(
                  newValue === '' || validation.PASSWORD_REGEX.test(newValue)
                    ? ''
                    : getText('passwordValidationError')
                )
              }}
            />
          </aria.TextField>
          <aria.TextField className="flex h-row gap-settings-entry">
            <aria.Label className="text my-auto w-change-password-settings-label">
              {getText('confirmNewPasswordLabel')}
            </aria.Label>
            <SettingsInput
              type="password"
              initialValue=""
              placeholder={getText('confirmNewPasswordPlaceholder')}
              onChange={event => {
                const newValue = event.currentTarget.value
                setConfirmNewPassword(newValue)
                event.currentTarget.setCustomValidity(
                  newValue === '' || newValue === newPassword
                    ? ''
                    : getText('passwordMismatchError')
                )
              }}
            />
          </aria.TextField>
          <ButtonRow>
            <UnstyledButton
              isDisabled={!canSubmitPassword}
              className={`settings-value rounded-full bg-invite font-medium text-white selectable enabled:active`}
              onPress={() => {
                setPasswordFormKey(uniqueString.uniqueString())
                setCurrentPassword('')
                setNewPassword('')
                setConfirmNewPassword('')
                void changePassword(currentPassword, newPassword)
              }}
            >
              {getText('change')}
            </UnstyledButton>
            <UnstyledButton
              isDisabled={!canSubmitPassword}
              className="settings-value rounded-full bg-selected-frame font-medium selectable enabled:active"
              onPress={() => {
                setPasswordFormKey(uniqueString.uniqueString())
                setCurrentPassword('')
                setNewPassword('')
                setConfirmNewPassword('')
              }}
            >
              {getText('cancel')}
            </UnstyledButton>
          </ButtonRow>
        </div>
      )}
    </FocusArea>
  )
}
