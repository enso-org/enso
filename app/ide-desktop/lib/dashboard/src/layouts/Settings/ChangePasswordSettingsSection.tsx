/** @file Settings section for changing password. */
import * as React from 'react'

import * as keyboardNavigationHooks from '#/hooks/keyboardNavigationHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as navigator2DProvider from '#/providers/Navigator2DProvider'

import SettingsInput from '#/layouts/Settings/SettingsInput'

import * as uniqueString from '#/utilities/uniqueString'
import * as validation from '#/utilities/validation'

// =====================================
// === ChangePasswordSettingsSection ===
// =====================================

/** Settings section for changing password. */
export default function ChangePasswordSettingsSection() {
  const { changePassword } = authProvider.useAuth()
  const navigator2D = navigator2DProvider.useNavigator2D()
  const [passwordFormKey, setPasswordFormKey] = React.useState('')
  const [currentPassword, setCurrentPassword] = React.useState('')
  const [newPassword, setNewPassword] = React.useState('')
  const [confirmNewPassword, setConfirmNewPassword] = React.useState('')
  const rootRef = React.useRef<HTMLDivElement>(null)

  const [keyboardSelectedIndex, setKeyboardSelectedIndex] =
    keyboardNavigationHooks.useKeyboardChildNavigation(rootRef, { length: 3 })

  React.useEffect(() => {
    const root = rootRef.current
    if (root == null) {
      return
    } else {
      return navigator2D.register(root, {
        focusPrimaryChild: setKeyboardSelectedIndex.bind(null, 0),
        focusWhenPressed: {
          up: setKeyboardSelectedIndex.bind(null, 2),
        },
      })
    }
  }, [navigator2D, setKeyboardSelectedIndex])

  const canSubmitPassword =
    currentPassword !== '' &&
    newPassword !== '' &&
    confirmNewPassword !== '' &&
    newPassword === confirmNewPassword &&
    validation.PASSWORD_REGEX.test(newPassword)

  return (
    <div key={passwordFormKey} ref={rootRef}>
      <h3 className="settings-subheading">Change Password</h3>
      <div className="flex h-row gap-settings-entry">
        <span className="text my-auto w-change-password-settings-label">Current Password</span>
        <span className="text my-auto grow font-bold">
          <SettingsInput
            type="password"
            ref={element => {
              if (keyboardSelectedIndex === 0) {
                element?.focus()
              }
            }}
            focusRing={keyboardSelectedIndex === 0}
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
            ref={element => {
              if (keyboardSelectedIndex === 1) {
                element?.focus()
              }
            }}
            focusRing={keyboardSelectedIndex === 1}
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
        <span className="text my-auto w-change-password-settings-label">Confirm New Password</span>
        <span className="text my-auto grow font-bold">
          <SettingsInput
            type="password"
            ref={element => {
              if (keyboardSelectedIndex === 2) {
                element?.focus()
              }
            }}
            focusRing={keyboardSelectedIndex === 2}
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
        <button
          type="submit"
          disabled={!canSubmitPassword}
          className={`settings-value rounded-full bg-invite font-medium text-white selectable enabled:active`}
          onClick={() => {
            setPasswordFormKey(uniqueString.uniqueString())
            setCurrentPassword('')
            setNewPassword('')
            setConfirmNewPassword('')
            void changePassword(currentPassword, newPassword)
          }}
        >
          Change
        </button>
        <button
          type="button"
          disabled={!canSubmitPassword}
          className="settings-value rounded-full bg-selected-frame font-medium selectable enabled:active"
          onClick={() => {
            setPasswordFormKey(uniqueString.uniqueString())
            setCurrentPassword('')
            setNewPassword('')
            setConfirmNewPassword('')
          }}
        >
          Cancel
        </button>
      </div>
    </div>
  )
}
