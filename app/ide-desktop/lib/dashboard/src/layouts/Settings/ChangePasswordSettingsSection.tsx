/** @file Settings section for changing password. */
import * as React from 'react'

import * as authProvider from '#/providers/AuthProvider'

import SettingsInput from '#/layouts/Settings/SettingsInput'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import * as uniqueString from '#/utilities/uniqueString'
import * as validation from '#/utilities/validation'

// =====================================
// === ChangePasswordSettingsSection ===
// =====================================

/** Settings section for changing password. */
export default function ChangePasswordSettingsSection() {
  const { changePassword } = authProvider.useAuth()
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
          <h3 className="settings-subheading">Change Password</h3>
          <div className="flex h-row gap-settings-entry">
            <span className="text my-auto w-change-password-settings-label">Current Password</span>
            <span className="text my-auto grow font-bold">
              <SettingsInput
                type="password"
                initialValue=""
                placeholder="Enter your current password"
                onChange={event => {
                  setCurrentPassword(event.currentTarget.value)
                }}
              />
            </span>
          </div>
          <div className="flex h-row gap-settings-entry">
            <span className="text my-auto w-change-password-settings-label">New Password</span>
            <span className="text my-auto grow font-bold">
              <SettingsInput
                type="password"
                initialValue=""
                placeholder="Enter your new password"
                onChange={event => {
                  const newValue = event.currentTarget.value
                  setNewPassword(newValue)
                  event.currentTarget.setCustomValidity(
                    newValue === '' || validation.PASSWORD_REGEX.test(newValue)
                      ? ''
                      : validation.PASSWORD_ERROR
                  )
                }}
              />
            </span>
          </div>
          <div className="flex h-row gap-settings-entry">
            <span className="text my-auto w-change-password-settings-label">
              Confirm New Password
            </span>
            <span className="text my-auto grow font-bold">
              <SettingsInput
                type="password"
                initialValue=""
                placeholder="Confirm your new password"
                onChange={event => {
                  const newValue = event.currentTarget.value
                  setConfirmNewPassword(newValue)
                  event.currentTarget.setCustomValidity(
                    newValue === '' || newValue === newPassword ? '' : 'Passwords must match.'
                  )
                }}
              />
            </span>
          </div>
          <div className="flex h-row items-center gap-buttons">
            <FocusRing>
              <aria.Button
                type="submit"
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
                Change
              </aria.Button>
            </FocusRing>
            <FocusRing>
              <aria.Button
                type="button"
                isDisabled={!canSubmitPassword}
                className="settings-value rounded-full bg-selected-frame font-medium selectable enabled:active"
                onPress={() => {
                  setPasswordFormKey(uniqueString.uniqueString())
                  setCurrentPassword('')
                  setNewPassword('')
                  setConfirmNewPassword('')
                }}
              >
                Cancel
              </aria.Button>
            </FocusRing>
          </div>
        </div>
      )}
    </FocusArea>
  )
}
